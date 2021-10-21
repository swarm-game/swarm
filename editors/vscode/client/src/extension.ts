import * as path from 'path';
import * as process from 'process';
import { workspace, ExtensionContext, DebugAdapterExecutable } from 'vscode';

import {
	Executable,
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TextDocumentSyncKind,
	TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	// The server is implemented in node
	const serverModule : Executable = {
		command: "swarm",
		args: ["lsp"],
		options: {detached:true, shell:true},
	  }
	

	var serverOptions: ServerOptions = serverModule;
	TextDocumentSyncKind

	// Options to control the language client
	const clientOptions: LanguageClientOptions = {
		// Register the server for swarm files
		documentSelector: [{ scheme: 'file', language: 'swarm' }]
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'languageServerSwarm',
		'Language Server Swarm',
		serverOptions,
		clientOptions
	);

	// Start the client. This will also launch the server
	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
