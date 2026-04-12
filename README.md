# dotemacsen (Emacs configuration)

Personal Emacs config focused on a fast startup, terminal-friendly defaults, and built-in-first tooling.

## Supported Emacs versions

- Required: **Emacs 30 or newer** (`init.el` errors out on older versions)
- Recommended: latest Emacs 30.x release

## What this config expects

- `git` (required by `straight.el` package manager)
- network access on first boot (to download package sources)
- optional CLI tools for language features:
  - `typescript-language-server` (TypeScript/TSX via Eglot)
  - `vscode-langservers-extracted` (JSON diagnostics/completion via Eglot)
  - `yaml-language-server` (YAML via Eglot)
  - `rust-analyzer` (Rust via Eglot)
- optional tree-sitter grammars for best major modes:
  - `javascript`, `json`, `typescript`, `tsx`, `rust`, `yaml`, `toml`

## Install (symlink to home)

### Recommended (install script)

From this repo directory:

```bash
./install.sh
```

What it does:

- backs up existing `~/.emacs.d` to `~/.emacs.d.backup.<timestamp>`
- creates a symlink from this repo to `~/.emacs.d`
- leaves existing backups untouched

### Manual install

From this repo directory:

```bash
# 1) backup existing Emacs config (if any)
[ -e ~/.emacs.d ] && mv ~/.emacs.d ~/.emacs.d.backup.$(date +%Y%m%d-%H%M%S)

# 2) symlink this repo as ~/.emacs.d
ln -s "$(pwd)" ~/.emacs.d

# 3) start Emacs
emacs
```

Alternative if you keep this repo elsewhere:

```bash
ln -s /absolute/path/to/dotemacsen ~/.emacs.d
```

## First run checklist

- Start Emacs once and wait for `straight.el` to finish bootstrapping packages.
- Restart Emacs after initial install.
- Verify package builds with `M-x straight-check-all`.
- If using LSP, install language servers listed above.
- If you want tree-sitter modes, install the grammars listed above.

## Helpful shortcuts

- VS Code `Ctrl+P` equivalent (quick open file): `C-x C-f`
- VS Code `Ctrl+Shift+P` equivalent (command palette): `M-x`
- Find file: `C-x C-f`
- Save file: `C-x C-s`
- Open/switch buffer: `C-x b`
- Kill current buffer: `C-x k`
- List buffers: `C-x C-b`
- Open command prompt (`M-x`): `M-x`
- Cancel current command: `C-g`
- Split window vertically: `C-x 2`
- Split window horizontally: `C-x 3`
- Move to other window: `C-x o`
- Close other windows: `C-x 1`
- Undo: `C-/`
- Redo: `C-S-/`
- Incremental search forward: `C-s`
- Incremental search backward: `C-r`
- Copy (kill region): `M-w`
- Cut (kill region): `C-w`
- Paste (yank): `C-y`
- View all active key bindings in current buffer: `C-h b` (`describe-bindings`)
- Open Dired: `C-x d` or `M-x dired`

## Updating this config

```bash
cd /path/to/dotemacsen
git pull
```

Then inside Emacs:

- `M-x straight-pull-all`
- `M-x straight-rebuild-all` (only if needed)

## Non-builtin packages used

These are installed from external sources (not bundled with Emacs):

- `straight.el`
- `use-package`
- `gcmh`
- `hydra`
- `restart-emacs`
- `diminish`
- `no-littering`
- `which-key`
- `avy`
- `expand-region`
- `highlight-indent-guides`
- `typescript-mode`
- `yaml-mode`
- `rust-mode`

Built-in packages/features are configured heavily as well (for example `eglot`, `flymake`, `fido-vertical-mode`, `savehist`, `dired`, etc.), but those ship with Emacs.
