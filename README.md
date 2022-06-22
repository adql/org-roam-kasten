# Org Roam Kasten

_Org Roam Kasten_ (_Ork_) puts your _zettel_ in the _kasten_. It is essentially a browser for your [Org-roam](https://www.orgroam.com/) notes, displaying one note at a time and allowing easy navigation.

_This is currently experimental_, **use at your own risk**. That being said, _Ork_ is a read-only tool that should by itself not modify your files.

## Installation and settings

_Org Roam Kasten_ requires Org-roam v2.

The usual. Have the package in your `load-path` and

``` emacs-lisp
(require 'org-roam-kasten)
```

<a name="entry-tags"></a>
The most basic setting is for entry nodes. These are defined in `ork-entry-tag-re` by a regular expression to match tags of nodes that you wish to appear in the _find_ minibuffer. The default behavior (`"^@.+"`) pulls all nodes that have a tag beginning with `@` (this is a rather arbitrary default that fit my usage when I first wrote the package; it may be changed in the future).

_Ork_ doesn't make any visual modifications to the browser buffer. It is recommended to add relevant minor modes to `org-roam-kasten-mode-hook` according to your preferences.

## Usage

The entry point to _Ork_ is with `ork-enter`. This will open an `org-roam-node-read` (like `find`, but for finding existing nodes only) minibuffer with the entry nodes [of your choice](#entry-tags).

Choose a node. A special read-only buffer named `*Zettelkasten*` will open, displaying that node with point at the folded heading. Here `TAB` has a special behavior: when first press, it will unfold the heading. Afterwards it will navigate (i.e. move point) between the links in the node, with `S-TAB` for navigating backwards. Press `ENT` to follow the link at point, all within the `*Zettelkasten*` buffer.

The other default keybindings are in spirit of Emacs' `Info` mode:

- `p` / `n`: navigate to previous/next `zettel` at the same level
- `[` / `]`: navigate to previous/next "physical" `zettel` – previous/next heading in the Org file, and if there is no such, then previous/next in a neighboring file according to alphabetic order.
- `l` / `r`: move back/forward in history
- `^`: navigate to parent `zettel`
- `o`: visit the node in a normal Org buffer
- `SPACE`: examine `folgezettel` in a special line in the buffer
- `C-c l`: store link to the displayed node with `org-store-link` (TODO: change this binding according to conventions)
- `q`: quit the window
