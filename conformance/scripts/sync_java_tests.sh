#!/bin/bash

# Script to sync with the latest Java JSLT repository and potentially extract new test cases
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
JAVA_JSLT_DIR="$SCRIPT_DIR/../java-jslt"

echo "Syncing Java JSLT repository..."

cd "$JAVA_JSLT_DIR"

# Get current commit hash
old_commit=$(git rev-parse HEAD)

# Pull latest changes
git pull origin master

# Get new commit hash
new_commit=$(git rev-parse HEAD)

if [ "$old_commit" = "$new_commit" ]; then
    echo "✅ Already up to date with Java JSLT repository"
else
    echo "📦 Updated Java JSLT from $old_commit to $new_commit"
    
    # Show what changed
    echo ""
    echo "Recent changes:"
    git log --oneline "$old_commit..$new_commit" | head -10
    
    echo ""
    echo "💡 Consider checking for new test cases or language features that should be added to our conformance suite."
    echo "💡 You may want to regenerate expected outputs with: ./regenerate_expected.sh"
fi

cd - > /dev/null