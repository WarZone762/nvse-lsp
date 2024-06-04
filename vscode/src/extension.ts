import { ExtensionContext, Uri } from "vscode";

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  const serverModule = Uri.joinPath(
    context.extensionUri,
    "dist",
    "nvse-lsp",
  ).fsPath;

  const serverOptions: ServerOptions = {
    run: { command: serverModule },
    debug: { command: serverModule },
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "nvsescript" }],
  };

  client = new LanguageClient(
    "nvse-lsp",
    "NVSE Language Server",
    serverOptions,
    clientOptions,
  );

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
