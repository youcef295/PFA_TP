#!/bin/sh

grep '#INIT TP OCAML' "$HOME"/.bashrc && exit 0

echo "source /public/kn/setup_ocaml.sh" >> "$HOME"/.bashrc

if ! test -d "$HOME/.vscode/extensions/ocamllabs.ocaml-platform-1.13.4";
then
    code --force --install-extension /public/kn/ocaml-platform.vsix >/dev/null 2>&1
fi

mkdir -p "$HOME"/.config/Code/User

if ! test -f "$HOME"/.config/Code/User/settings.json;
then
    cp /public/kn/settings.json "$HOME"/.config/Code/User/settings.json;
fi

echo '#INIT TP OCAML' >> "$HOME"/.bashrc

