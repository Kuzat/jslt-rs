import * as path from 'path';
import {workspace, ExtensionContext, window} from "vscode";
import {
  Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";
import * as fs from "node:fs";

let client: LanguageClient;

function getServerPath(context: ExtensionContext): string | undefined {
  // Check user configuration first
  const config = workspace.getConfiguration('jslt');
  const configPath = config.get<string>('serverPath');

  if (configPath && fs.existsSync(configPath)) {
    return configPath;
  }

  // try bundled binary
  const binaryName = process.platform === 'win32' ? 'jslt-lsp.exe' : 'jslt-lsp';
  const bundlesPath = path.join(context.extensionPath, 'bin', binaryName)

  if (fs.existsSync(bundlesPath)) {
    return bundlesPath;
  }

  return undefined;
}

export function activate(context: ExtensionContext) {
  const serverPath = getServerPath(context);
  console.info(`using server path: ${serverPath}`);

  if (!serverPath) {
    console.error("serverPath is undefined");
    window.showErrorMessage(
      'JSLT LSP server not found. Please install the extension properly or set jslt.serverPath in settings.',
      'Open Settings'
    ).then(selection => {
      if (selection === 'Open Settings') {
        workspace.getConfiguration('jslt').update('serverPath', '', true);
      }
    });
    return;
  }

  if (process.platform !== "win32") {
    try {
      fs.chmodSync(serverPath, 0o755);
    } catch (e) {
      console.error(e);
      window.showErrorMessage(
        `Failed to set executable permissions on JSLT LSP server: ${e}`,
        'Open Settings'
      ).then(selection => {
        if (selection === 'Open Settings') {
          workspace.getConfiguration('jslt').update('serverPath', '', true);
        }
      });
    }
  }

  const run: Executable = {
    command: serverPath,
    options: {
      env: {
        ...process.env,
        RUST_LOG: "info",
      }
    }
  };

  // Server options
  const serverOptions: ServerOptions = {
    run,
    debug: run,
  };

  // Client options
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{scheme: 'file', language: 'jslt'}],
    synchronize: {
      // Notify the server about file changes to .jslt files
      fileEvents: workspace.createFileSystemWatcher('**/*.jslt'),
    },
  };

  // Create and start the language client
  client = new LanguageClient(
    'jsltLanguageServer',
    'JSLT Language Server',
    serverOptions,
    clientOptions
  );

  // start the client (which launches the server)
  client.start();

  context.subscriptions.push({
    dispose: () => {
      if (client) {
        client.stop();
      }
    }
  })
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}