//! [`Map`]: the entries of a JSON object, in insertion order.

use crate::JsltValue;
use hashbrown::HashTable;
use std::fmt;
use std::hash::BuildHasher;
use std::sync::{Arc, OnceLock};

/// Above this many entries, lookups go through a hash index instead of a linear scan.
const INDEX_THRESHOLD: usize = 8;

/// Object entries in insertion order. Keys are shared `Arc<str>`s, so copying a key into
/// another object does not allocate.
///
/// Most JSON objects are small, so a map is a flat list that is searched linearly, which needs
/// one allocation and no hashing. The first lookup in a larger map builds an index of entry
/// positions, hashed with foldhash and a seed that is random per process. Maps that are only
/// built and iterated never hash their keys.
#[derive(Clone, Default)]
pub struct Map {
    entries: Vec<(Arc<str>, JsltValue)>,
    index: OnceLock<HashTable<usize>>,
}

fn hash_key(key: &str) -> u64 {
    static STATE: OnceLock<foldhash::fast::RandomState> = OnceLock::new();
    STATE.get_or_init(Default::default).hash_one(key)
}

impl Map {
    pub fn new() -> Self {
        Self::default()
    }

    /// An empty map with room for `n` entries.
    pub fn with_capacity(n: usize) -> Self {
        Map { entries: Vec::with_capacity(n), index: OnceLock::new() }
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    fn position(&self, key: &str) -> Option<usize> {
        if self.entries.len() <= INDEX_THRESHOLD {
            return self.entries.iter().position(|(k, _)| &**k == key);
        }
        let index = self.index.get_or_init(|| self.build_index());
        index.find(hash_key(key), |&i| &*self.entries[i].0 == key).copied()
    }

    fn build_index(&self) -> HashTable<usize> {
        let mut index = HashTable::with_capacity(self.entries.capacity());
        for (i, (k, _)) in self.entries.iter().enumerate() {
            index.insert_unique(hash_key(k), i, |&j| hash_key(&self.entries[j].0));
        }
        index
    }

    pub fn get(&self, key: &str) -> Option<&JsltValue> {
        self.position(key).map(|i| &self.entries[i].1)
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut JsltValue> {
        self.position(key).map(|i| &mut self.entries[i].1)
    }

    pub fn contains_key(&self, key: &str) -> bool {
        self.position(key).is_some()
    }

    /// Sets `key` to `value`. A key that is already present keeps its position, and its old
    /// value is returned.
    pub fn insert(&mut self, key: Arc<str>, value: JsltValue) -> Option<JsltValue> {
        let existing = if self.entries.len() <= INDEX_THRESHOLD {
            self.entries.iter().position(|(k, _)| Arc::ptr_eq(k, &key) || *k == key)
        } else {
            self.position(&key)
        };
        if let Some(i) = existing {
            return Some(std::mem::replace(&mut self.entries[i].1, value));
        }
        self.insert_unique(key, value);
        None
    }

    /// Appends an entry whose key is not in the map. This skips the search that
    /// [`insert`](Self::insert) does. A duplicate key gives a map with two entries for it.
    pub fn insert_unique(&mut self, key: Arc<str>, value: JsltValue) {
        debug_assert!(!self.contains_key(&key), "duplicate key {key}");
        let i = self.entries.len();
        let hash = self.index.get().map(|_| hash_key(&key));
        self.entries.push((key, value));
        if let (Some(index), Some(hash)) = (self.index.get_mut(), hash) {
            let entries = &self.entries;
            index.insert_unique(hash, i, |&j| hash_key(&entries[j].0));
        }
    }

    /// Reserves room for `n` more entries.
    pub fn reserve(&mut self, n: usize) {
        self.entries.reserve(n);
        if let Some(index) = self.index.get_mut() {
            let entries = &self.entries;
            index.reserve(n, |&j| hash_key(&entries[j].0));
        }
    }

    pub fn clear(&mut self) {
        self.entries.clear();
        self.index.take();
    }

    pub fn get_index(&self, i: usize) -> Option<(&Arc<str>, &JsltValue)> {
        self.entries.get(i).map(|(k, v)| (k, v))
    }

    pub fn get_index_mut(&mut self, i: usize) -> Option<(&Arc<str>, &mut JsltValue)> {
        self.entries.get_mut(i).map(|(k, v)| (&*k, v))
    }

    pub fn first(&self) -> Option<(&Arc<str>, &JsltValue)> {
        self.get_index(0)
    }

    pub fn iter(&self) -> Iter<'_> {
        Iter(self.entries.iter())
    }

    pub fn keys(&self) -> impl DoubleEndedIterator<Item = &Arc<str>> + ExactSizeIterator {
        self.entries.iter().map(|(k, _)| k)
    }

    pub fn values(&self) -> impl DoubleEndedIterator<Item = &JsltValue> + ExactSizeIterator {
        self.entries.iter().map(|(_, v)| v)
    }
}

/// Iterator over the entries of a [`Map`], in order.
pub struct Iter<'a>(std::slice::Iter<'a, (Arc<str>, JsltValue)>);

impl<'a> Iterator for Iter<'a> {
    type Item = (&'a Arc<str>, &'a JsltValue);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(k, v)| (k, v))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl DoubleEndedIterator for Iter<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.0.next_back().map(|(k, v)| (k, v))
    }
}

impl ExactSizeIterator for Iter<'_> {}

impl<'a> IntoIterator for &'a Map {
    type Item = (&'a Arc<str>, &'a JsltValue);
    type IntoIter = Iter<'a>;

    fn into_iter(self) -> Iter<'a> {
        self.iter()
    }
}

impl IntoIterator for Map {
    type Item = (Arc<str>, JsltValue);
    type IntoIter = std::vec::IntoIter<(Arc<str>, JsltValue)>;

    fn into_iter(self) -> Self::IntoIter {
        self.entries.into_iter()
    }
}

impl Extend<(Arc<str>, JsltValue)> for Map {
    fn extend<I: IntoIterator<Item = (Arc<str>, JsltValue)>>(&mut self, iter: I) {
        let iter = iter.into_iter();
        self.reserve(iter.size_hint().0);
        for (k, v) in iter {
            self.insert(k, v);
        }
    }
}

impl FromIterator<(Arc<str>, JsltValue)> for Map {
    fn from_iter<I: IntoIterator<Item = (Arc<str>, JsltValue)>>(iter: I) -> Self {
        let mut map = Map::new();
        map.extend(iter);
        map
    }
}

/// Maps are equal when they hold the same keys with equal values, in any order.
impl PartialEq for Map {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().all(|(k, v)| other.get(k) == Some(v))
    }
}

impl fmt::Debug for Map {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn key(i: usize) -> Arc<str> {
        format!("k{i}").into()
    }

    fn num(i: i64) -> JsltValue {
        JsltValue::Number(i.into())
    }

    #[test]
    fn keeps_order_and_replaces_in_place_small_and_large() {
        for n in [3, INDEX_THRESHOLD, INDEX_THRESHOLD + 1, 100] {
            let mut m = Map::new();
            for i in 0..n {
                assert_eq!(m.insert(key(i), num(i as i64)), None);
            }
            assert_eq!(m.insert(key(1), num(-1)), Some(num(1)));
            assert_eq!(m.len(), n);
            assert!(m.keys().map(|k| k.to_string()).eq((0..n).map(|i| format!("k{i}"))));
            assert_eq!(m.get("k1"), Some(&num(-1)));
            for i in 0..n {
                assert!(m.contains_key(&format!("k{i}")), "k{i} of {n}");
            }
            assert!(!m.contains_key("missing"));
            let mut reversed: Map = m.iter().rev().map(|(k, v)| (k.clone(), v.clone())).collect();
            assert_eq!(reversed, m);
            reversed.insert(key(0), JsltValue::Null);
            assert_ne!(reversed, m);
        }
    }

    #[test]
    fn reserve_and_extend_keep_index_valid() {
        let mut m = Map::new();
        m.extend((0..20).map(|i| (key(i), JsltValue::Null)));
        m.reserve(1000);
        m.extend((20..40).map(|i| (key(i), JsltValue::Null)));
        assert!((0..40).all(|i| m.contains_key(&format!("k{i}"))));
        m.clear();
        assert!(m.is_empty() && !m.contains_key("k0"));
    }

    #[test]
    fn insert_unique_before_and_after_the_index_is_built() {
        let mut m = Map::new();
        (0..20).for_each(|i| m.insert_unique(key(i), num(i as i64)));
        assert_eq!(m.get("k15"), Some(&num(15)));
        (20..30).for_each(|i| m.insert_unique(key(i), num(i as i64)));
        assert!((0..30).all(|i| m.get(&format!("k{i}")) == Some(&num(i as i64))));
        assert_eq!(m.insert(key(25), JsltValue::Null), Some(num(25)));
        assert_eq!(m.len(), 30);
    }
}
