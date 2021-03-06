;;; one-key.el --- Easy access configurable popup menus to display keybindings and other things.

;; Filename: one-key.el
;; Description: One key
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;;         rubikitch <rubikitch@ruby-lang.org>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2012, Joe Bloggs, all rites reversed.
;; Copyright (C) 2008, 2009, 2010 Andy Stewart, all rights reserved.
;; Copyright (C) 2009, rubikitch, all rights reserved.
;; Created: 2008-12-22 21:54:30
;; Version: 1.0
;; Last-Updated: 2/8/2012 16:00:00
;;           By: Joe Bloggs
;; URL: http://www.emacswiki.org/emacs/download/one-key.el
;; Keywords: one-key
;; Compatibility: GNU Emacs 22 ~ 23
;;
;; Features that might be required by this library:
;;
;; `cl' `hexrgb' `reporter' `browse-url'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; With so many Emacs extensions, you have a lot of keystrokes to remember, and you probably forget most of them.
;;
;; This package fixes that problem, and helps new users to learn the common keybindings.
;;
;; One Key provides a single keystroke that when pressed presents you with a menu of choices in a popup window
;; for commands to execute with a further keystroke. By default menus for common prefix keys and commands are defined.
;;
;; Just type one of the listed keystrokes to execute the corresponding command.
;;
;; You can delete, edit, sort, highlight and filter the menu items, add new menu items and even add new menus.
;; You can have access to several different menus from the same window which can be navigated with the arrow keys.
;; Such a collection of menus is called a menu set and you can define several different menu sets containing different
;; types of menus.
;; Several different types of menus are defined (and more may be added) for holding different types of menu items.
;; For example the "major-mode" type opens the menu corresponding to the current major-mode.
;; More different types are defined by one-key extension libraries (e.g. `one-key-dir' for fast directory
;; tree navigation), or you can create your own types. See "Creating menus" below.

;;; The *One-Key* buffer:
;;
;; Running the command `one-key-open-default-menu-set' or `one-key-open-menu-set' opens the *One-Key* buffer.
;; (these commands may be bound to keys - see "Installation" below).
;;
;; Within the *One-Key* buffer you will see a list of command descriptions each with a corresponding key in square
;; brackets to its left. Pressing the key executes the command.
;; Along the top of the buffer in the header line you will see a list of menu names. One of these names will be
;; highlighted and indicates the current menu. You can navigate between the different menus by pressing the left/right
;; arrow keys (unless these have been redefined to other keys in `one-key-special-keybindings', see below).
;;
;; You can toggle the size of the window holding the *One-Key* buffer by pressing the appropriate special key (see below).
;; The window size is toggled between default size, large size (so that all items fit in the window), and hidden (window
;; is closed, but one-key is still active). This is useful if there are a large number of items in the menu, or when you
;; need to see part of the buffer that is obscured by the *One-Key* window.
;;
;; By default one-key will quit and the *One-Key* window will close after pressing a key corresponding to one of the
;; menu items. If you want one-key to stay active after pressing an item key you should toggle the menu persistence
;; by pressing the appropriate special key (C-menu by default, see "Special keybindings" below).
;; To quit one-key and close the *One-Key* window press ESC. If you want to quit but keep the window open (e.g. to see
;; the keybindings for a major mode), press C-ESC.

;;; Special keybindings:
;;
;; For each different type of menu certain "special" keybindings are defined which activate menu specific commands,
;; such as sorting or editing the menu items, adding new menus, etc.
;; These special keybindings are specific to each menu type, though many of them will be the same for all menu types.
;; For example the arrow keys are defined as special keybindings for navigating around menus.
;;
;; Pressing the f1 key displays a help message listing all the special keybindings for the current menu.
;;
;; By default, when a menu is created one-key will ensure that the keys corresponding to menu items do not clash with
;; the special keybindings for that menu type. However, if for some reason there is a clash then the menu item gets
;; priority over the special keybinding unless the help window is displayed (by pressing f1), in which case the special
;; keybinding gets priority.
;;
;; You can alter or add new special keybindings by customizing `one-key-special-keybindings',
;; and `one-key-default-special-keybindings'.
;; Extension libraries (such as `one-key-dir' or `one-key-regs') may also define customizable special keys specific
;; to the menu type defined in the library.
;;
;; By default the following special keybindings are defined:
;;
;; ESC        : Quit and close menu window                       
;; <C-escape> : Quit, but keep menu window open                  
;; <C-menu>   : Toggle menu persistence                          
;; <menu>     : Toggle menu display                              
;; <left>     : Change to next menu                              
;; <right>    : Change to previous menu                          
;; <up>       : Scroll/move up one line                          
;; <down>     : Scroll/move down one line                        
;; <prior>    : Scroll menu down one page                        
;; <next>     : Scroll menu up one page                          
;; C-h        : Show help for next item chosen                   
;; C-s        : Save current state of menu                       
;; <f1>       : Toggle this help buffer                          
;; <f2>       : Toggle column/row ordering of items              
;; <f3>       : Sort items by next method
;; <C-f3>     : Sort items by previous method          
;; <f4>       : Reverse order of items                 
;; /          : Limit items to those matching regexp   
;; C-/        : Highlight items matching regexp        
;; <f5>       : Edit a menu item                       
;; <f6>       : Delete a menu item                     
;; <f7>       : Copy/kill coloured items               
;; <C-f7>     : Yank copied items                      
;; <f8>       : Swap menu item keys                    
;; <f9>       : Add a menu item                        
;; <C-f9>     : Add a menu                             
;; <C-S-f9>   : Remove this menu                       
;; <f10>      : Reposition item (with arrow keys)      
;; <f11>      : Donate to support further development  
;; <C-f11>    : Report a bug                           

;;; Creating menus:
;; 
;; All of the menus are stored in the file `one-key-menus-save-file' (customizable), but you should never need to
;; edit this file. Instead you can create and edit menus from within the *One-Key* buffer.
;; If you press the special key corresponding to "Add a menu" you will be prompted for the type of menu to add.
;; By default the following menu types are defined, but more may be added with extension libraries, or by creating
;; them yourself (see `one-key-types-of-menu'):

;;; Default menu types:
;;
;; top-level         : contains items defined in `one-key-menu-toplevel-alist', which by default contains common prefix key
;;                     menus, and menus for common commands to help new users learn emacs
;; blank menu        : creates a blank menu with no items
;; major-mode        : contains items corresponding to the current major mode (keybindings and menu-bar items)
;; existing menu     : prompts for an existing menu to use
;; existing keymap   : contains items in a given keymap (prompted for)
;; prefix key keymap : contains items whose usual keybindings begin with a given prefix key (prompted for)
;; menu-sets         : contains items for opening menu sets (see below)

;;; Saving menus:

;; If `one-key-autosave-menus' is non-nil then any new menus or menus that have been changed will be saved
;; on Emacs exit, unless they are listed in `one-key-exclude-from-save'
;; Alternatively you may save individual menus by pressing the special key for "Save the current state of menu"
;; (C-s by default).

;;; Menu sets:

;; A menu set is a collection of menu names. When you open the *One-Key* buffer with `one-key-open-default-menu-set'
;; it opens the default set of menus `one-key-default-menu-set'. You can define other sets of menus by customizing
;; `one-key-sets-of-menus-alist'. Each menu set consists of a name for the menu set, and a list of menu names.
;; one-key reconstructs a menu from its name by searching `one-key-types-of-menu' for a matching entry, and applying
;; the associated function to create the menu.
;; You can see what menu sets are currently defined, and switch menu sets, using the "menu-sets" menu.
;; See "Creating menus" above for info on how to add the "menu-sets" menu to the *One-Key* buffer.

;;; Other features:
;;
;; Item help: to get help on a particular menu item press C-h followed by the key for the item. The help page
;; for the associated command will be displayed. If part of the *Help* buffer is obscured by the *One-Key* buffer,
;; you can hide the *One-Key* buffer by pressing the special key to toggle the display (<menu> by default).
;;
;; Item brightening: By default the background colour of a menu item is increased relative to the brightness
;; of other items each time the item is executed. This means you can quickly find the most frequently used items
;; in a large menu. To turn this feature off set `one-key-auto-brighten-used-keys' to nil.
;;
;; Sorting/ordering: you can sort the items in a menu by pressing the appropriate special key, and add new sort
;; methods by customizing `one-key-default-sort-method-alist'. The current sort method is displayed in the mode-line,
;; however this information is not guaranteed to be correct when the *One-Key* menu is initially opened.
;; You can also toggle between row/column ordering of items, and reverse the order of items.
;; By trying out different sort and ordering combinations you can find a configuration which is most readable,
;; or fits most items on screen. The order of menu items is persistent between sessions if the menus are saved
;; (see "Saving menus").
;;
;; Filtering and colouring items: you can filter the items displayed to match a regular expression, or specify
;; the background colour of items that match a given regular expression (press f1 to see which key to press).
;; This can make the menus more readable. The background colours will be saved with the menu.
;;
;; Editing menus: you can add, delete and edit menu items, and also copy or kill (i.e. cut) and yank (i.e. paste)
;; menu items from one menu to another. To copy or kill a bunch of items first make sure they are all highlighted
;; with the same background colour (doesn't matter if they have different brightness levels), and then press the
;; appropriate special key (press f1 for help). You will be prompted for an item in the group, and whether or not
;; you want to also kill (cut) the items from the current menu. The items will be saved in `one-key-copied-items' and
;; can be then be yanked (pasted) into another menu.
;; Any further copy/kills will overwrite the value of `one-key-copied-items', and you cannot retrieve previous kills
;; so take care.
;; You can also reposition items in a menu:
;;    1) press the appropriate specialkey
;;    2) press the key of the item to be moved
;;    3) use the up/down arrow keys to move the item
;;    4) exit one-key to fix the item
;;
;; Support further development: writing this code required a significant amount of unpaid labour on my part.
;; Please consider donating to help support further development by pressing f11 in the *One-Key* menu.
;; To report a bug press C-f11. Please report the circumstances in which the bug occured (where you creating a new
;; menu? what major-mode was in use at the time? etc.).


;;; Installation:
;;
;; Put one-key.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Make sure that you also have hexrgb.el in your load-path.
;; At the time of writing it can be obtained from here: http://emacswiki.org/emacs/hexrgb.el

;; Add the following to your ~/.emacs startup file, replacing <menu> with whatever key you
;; want to use to open the *One-Key* buffer.
;;
;; (require 'one-key)
;; (global-set-key (kbd "<menu>") 'one-key-open-default-menu-set)
;;
;; You can define new menu sets by customizing `one-key-sets-of-menus-alist', and change the default
;; menu set by customizing `one-key-default-menu-set'.
;; You can try out new menus by pressing the special key for "Add a menu" from the *One-Key* buffer
;; (press f1 in the *One-Key* buffer to see a list of all special keybindings).
;;
;; Because this library uses a special implementation,
;; sometimes a `max-lisp-eval-depth' or `max-specpdl-size' error can occur.
;; So making the above two variables larger will reduce the probability that an error occurs.
;; E.g:
;;
;; (setq max-lisp-eval-depth 10000)
;; (setq max-specpdl-size 10000)
;;

;;; Bug reporting
;;
;; To report a bug: M-x one-key-submit-bug-report, or press the appropriate special key 

;;; Customize:

;; `one-key-default-menu-keys' : A list of chars which may be used as the default keys in automatically generated 
;;                               `one-key' menus.
;; `one-key-min-keymap-submenu-size' : The minimum number of elements allowed in a submenu when creating menus from 
;;                                     keymaps.
;; `one-key-popup-window' : Whether to popup window when `one-key-menu' is run for the first time.
;; `one-key-buffer-name' : The buffer name of the popup menu window.
;; `one-key-column-major-order' : If true then menu items are displayed in column major order, otherwise row major order.
;; `one-key-force-multi-column-keymap-menus' : If non-nil then one-key menus created from keymaps will have command 
;;                                             descriptions shortened to fit two columns.
;; `one-key-menu-window-max-height' : The max height of popup menu window.
;; `one-key-menus-save-file' : The file where `one-key' menus are saved.
;; `one-key-autosave-menus' : If non-nil then one-key menus will automatically be saved when created or changed.
;; `one-key-exclude-from-save' : List of regular expressions matching names of menus which should not be autosaved.
;; `one-key-include-menubar-items' : Whether or not to include menu items with no keybinding when creating one-key menus 
;;                                   from keymaps.
;; `one-key-item-foreground-colour' : Foreground colour of highlighted items in `one-key' menus.
;; `one-key-auto-brighten-used-keys' : If non-nil then set brightness of menu items colours according to how often the 
;;                                     keys are pressed.
;; `one-key-submenus-replace-parents' : If non-nil then when a submenu of a `one-key' menu is opened it will replace the 
;;                                      parent menu.
;; `one-key-major-mode-remap-alist' : A list of cons cells mapping major modes to one-key-menus.
;; `one-key-menu-toplevel-alist' : The `one-key' top-level alist.
;; `one-key-sets-of-menus-alist' : Saved menu sets (sets of menus).
;; `one-key-default-menu-set' : The default menu set. It's value should be the car of one of the items in 
;;                              `one-key-sets-of-menus-alist'.
;; `one-key-default-sort-method-alist' : An alist of sorting methods to use on the `one-key' menu items.
;; `one-key-special-keybindings' : An list of special keys; labels, keybindings, descriptions and associated functions.
;; `one-key-default-special-keybindings' : List of special keys to be used if no other set of special keys is defined for 
;;                                         a given one-key menu type.
;; `one-key-menu-sets-special-keybindings' : List of special keys to be used for menu-sets menus (see 
;;                                           `one-key-default-special-keybindings' for more info).
;; `one-key-disallowed-keymap-menu-keys' : List of keys that should be excluded from one-key menus created from keymaps.
;; `one-key-types-of-menu' : A list of names of different types of `one-key' menu, and associated functions.
;; `one-key-persistent-menu-number' : If non-nil then when the default menu set is opened it will start with the same 
;;                                    menu as when previously opened.
;; `one-key-mode-line-message' : Form that when evaluated should produce a string for the mode-line in the *One-Key* 
;;                               buffer.

;; All above options can be customized through:
;;      M-x customize-group RET one-key RET
;;

;;; Change log:
;;
;; 2012/07/04
;;    * Joe Bloggs
;;       * This Change log is not being maintained anymore. Refer to the git log instead.
;; 2012/04/05
;;    * Joe Bloggs
;;       * Lots of changes! I have not been keeping track of them all.
;;       * Removed `one-key-items-per-line'
;;
;; 2012/3/01
;;    * Joe Bloggs
;;       * Lots of changes! Improved menu layout (can fit in more items), different menu sorting options,
;;       * colourization of menu items, limit items to those matching regexp, edit menu items in place,
;;       * manual repositioning of menu items in place.
;; 2010/12/07
;;    * Joe Bloggs
;;       * Added key-binding ("C-/" by default) to jump to source file of current one-key menu for editing.
;;       * Made fixed menu keys configurable with variables `one-key-key-hide' `one-key-key-quit' `one-key-key-up'
;;         `one-key-key-down' `one-key-key-pgup' `one-key-key-pgdown' `one-key-key-help' `one-key-key-edit'
;;         (they are called one-key-key-??? instead of one-key-???-key so that they will group together in the
;;          customization buffer).
;;       * Deleted `one-key-highlight-prompt' function since this is not used anywhere.
;;       * Added new variable `one-key-column-major-order', and altered `one-key-menu-format' function so that
;;         now you can choose whether items should be listed column first or row first.
;;
;; 2010/11/27
;;    * Joe Bloggs
;;       * Quick fix to one-key-template-write so that it remains in one-key-template-mode after writing
;;       
;; 2010/11/23
;;    * Joe Bloggs
;;       * Added `one-key-template-group-key-items-by-regexps', `one-key-template-describe-command',
;;         and associated keybindings and menu items.
;;
;; 2010/11/20
;;    * Joe Bloggs
;;       * Added `one-key-template-write' function for saving *One-Key-Template* buffer in `one-key-menus-location',
;;         and added keybinding `one-key-template-mode' and item to `one-key-menu-one-key-template-alist'.
;;       
;; 2010/11/18
;;    * Joe Bloggs
;;       * Added new major mode for editing one-key-menus in *One-Key-Template* buffer
;;       * Added following functions to aid editing menus in *One-Key-Template* buffer:
;;          `one-key-template-mode', `one-key-template-move-line-region', `one-key-template-move-line-region-up'
;;          `one-key-template-move-line-region-down', `one-key-template-test-menu', `one-key-template-mark-key-items'
;;          `one-key-template-sort-key-items-by-command-alphabetically',
;;          `one-key-template-sort-key-items-by-description-alphabetically',
;;          `one-key-template-sort-key-items-by-key-alphabetically',
;;          `one-key-menu-one-key-template', `one-key-menu-one-key'
;;       * Added keybindings for `one-key-template-mode'.
;;       * Altered `one-key-menu-format' function so that the keys are ordered by column instead of by row.
;;       * Added `one-key-toplevel-alist' customizable variable and `one-key-menu-toplevel' function.
;;       * Added `one-key-mode-alist' customizable variable and `one-key-get-menu' function.
;;       * Alterend `one-key-insert-template' and `one-key-show-template' functions so that they also add
;;         optional (commented) code to add items to `one-key-mode-alist' and `one-key-toplevel-alist'
;;       * Added customization variables `one-key-menus-location', `one-key-menus-regexp' and
;;         `one-key-auto-load-menus', and function `one-key-load-files'.
;;         Added code to automatically load menus if `one-key-auto-load-menus' is set to t.
;;       * Fixed spelling mistakes in documentation and added documentation for new features.
;;
;; 2010/09/27
;;    * Joe Bloggs
;;       * Altered one-key-make-template so that it adds the original keys to the descriptions of each item.
;;       
;; 2010/09/21
;;    * Joe Bloggs
;;       * Fixed a problems with one-key-make-template so it should work with more keymaps
;;       * Added ability to get help on one-key-menu items by pressing C-? followed by item key
;;       * Altered header text of menu
;;       * Fixed bug in one-key-menu so that window pops up if one-key-popup-window is t
;;         (this was also fixed independently by Andy, but I'm keeping my fix since it works fine)
;;
;; 2009/03/09
;;   * Andy Stewart:
;;      * Add `char-valid-p' for compatibility Emacs 22.
;;
;; 2009/02/25
;;   * Andy Stewart:
;;      * Fix a bug of `one-key-menu'.
;;
;; 2009/02/19
;;   * Andy Stewart:
;;      * Just show help message when first call function `one-key-menu',
;;        don't overwritten message from command.
;;      * Remove function `one-key-menu-quit' and
;;        option `one-key-show-quit-message', unnecessary now.
;;
;; 2009/02/10
;;   * rubikitch
;;      * Fix bug.
;;      * PageUp and PageDown are scroll page keys now.
;;      * Add new option `one-key-show-quit-message'.
;;
;; 2009/01/28
;;   * Andy Stewart:
;;      * Capitalize describe in variable `one-key-menu-*-alist'.
;;
;; 2009/01/27
;;   * rubikitch
;;      * Fix doc.
;;
;; 2009/01/26
;;   * rubikitch
;;      * Improve code.
;;
;; 2009/01/25
;;   * Andy Stewart:
;;      * Applied rubikitch's patch for generate
;;        template code automatically, very nice!
;;
;; 2009/01/22
;;   * rubikitch:
;;      * Add new option `one-key-items-per-line'.
;;      * Refactory code make it more clear.
;;      * Fix bug.
;;   * Andy Stewart:
;;      * Applied rubikitch's patch. Thanks!
;;      * Modified code make build-in keystroke
;;        can be overridden.
;;      * Fix doc.
;;
;; 2009/01/20
;;   * Andy Stewart:
;;      * Add new option `execute-last-command-when-miss-match'
;;        to function `one-key-menu', make user can execute
;;        last input command when miss match key alist.
;;
;; 2009/01/15
;;   * rubikitch:
;;      * Fix bug of `one-key-menu'.
;;      * Add recursion execute support for `one-key-menu'.*
;;        Thanks rubikitch patched for this! ;)
;;
;; 2009/01/04
;;   * Andy Stewart:
;;      * Add `alternate-function' argument with function `one-key-menu'.
;;
;; 2008/12/22
;;   * Andy Stewart:
;;      * First released.
;;

;;; Acknowledgements:
;;
;;      rubikitch <rubikitch@ruby-lang.org>
;;              For send many patches.
;;

;;; TODO
;;
;; New special keybinding for limiting by regexp all items in current menu and all submenus?
;; Make functions autoloadable.
;; Prompt to save submenus when saving menu. Special keybinding to save all altered menus?
;; Autohighlighting of menu items using regexp associations?
;; 
;;; Require
(eval-when-compile (require 'cl))
(require 'hexrgb)
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup one-key nil
  "One key - easy access, refactorable menus."
  :group 'editing)

(defcustom one-key-default-menu-keys
  (let (letters-and-numbers)
    (dotimes (i 26)
      (push (- ?Z i) letters-and-numbers))
    (dotimes (i 26)
      (push (- ?z i) letters-and-numbers))
    (dotimes (i 10)
      (push (- ?9 i) letters-and-numbers))
    letters-and-numbers)
  "A list of chars which may be used as the default keys in automatically generated `one-key' menus.
This list will be used for generating keys by the `one-key-generate-key' function."
  :group 'one-key
  :type '(repeat character))

(defcustom one-key-min-keymap-submenu-size 4
  "The minimum number of elements allowed in a submenu when creating menus from keymaps.
When creating menus from keymaps with `one-key-create-menus-from-keymap', submenus will be created for any prefix keys in
the keymap. These submenus contain the commands whose keybindings start with the corresponding prefix key.
If the number of items in a submenu would be less than `one-key-min-keymap-submenu-size' then instead of creating a submenu,
those items will be merged with the parent menu instead."
  :type 'integer
  :group 'one-key)

(defcustom one-key-popup-window t
  "Whether to popup window when `one-key-menu' is run for the first time."
  :type 'boolean
  :group 'one-key)

(defcustom one-key-buffer-name "*One-Key*"
  "The buffer name of the popup menu window."
  :type 'string
  :group 'one-key)

(defcustom one-key-column-major-order t
  "If true then menu items are displayed in column major order, otherwise row major order.
In column major order items will fill first column, then second, etc.
In row major order the rows are filled one at a time."
  :type 'boolean
  :group 'one-key)

(defcustom one-key-force-multi-column-keymap-menus t
  "If non-nil then one-key menus created from keymaps will have command descriptions shortened to fit two columns.
If nil then command descriptions will be allowed to fit the entire width of the menu if necessary in which case the
menu will contain only a single column.

Menus created from keymaps include major-mode menus and prefix-key menus."
  :type 'boolean
  :group 'one-key)

(defcustom one-key-menu-window-max-height nil
  "The max height of popup menu window."
  :type 'int
  :set (lambda (symbol value)
         (set symbol value)
         ;; Default is half height of frame.
         (unless value
           (set symbol (/ (frame-height) 2))))
  :group 'one-key)

(defcustom one-key-menus-save-file "~/.emacs.d/one-key-menus-save-file.el"
  "The file where `one-key' menus are saved."
  :type 'file
  :group 'one-key)

(defcustom one-key-autosave-menus nil
  "If non-nil then one-key menus will automatically be saved when created or changed."
  :type 'boolean
  :group 'one-key)

(defcustom one-key-exclude-from-save '("Prefix-Key:")
  "List of regular expressions matching names of menus which should not be autosaved."
  :type '(repeat (regexp :tag "Regexp" :help-echo "Regular expression matching menu names to exclude from autosave." ))
  :group 'one-key)

(defcustom one-key-include-menubar-items t
  "Whether or not to include menu items with no keybinding when creating one-key menus from keymaps."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)
                 (const :tag "Prompt each time" 'prompt))
  :group 'one-key)

(defcustom one-key-item-foreground-colour "black"
  "Foreground colour of highlighted items in `one-key' menus."
  :type 'color
  :group 'one-key)

(defcustom one-key-auto-brighten-used-keys t
  "If non-nil then set brightness of menu items colours according to how often the keys are pressed."
  :type 'boolean
  :group 'one-key)

(defcustom one-key-submenus-replace-parents nil
  "If non-nil then when a submenu of a `one-key' menu is opened it will replace the parent menu.
Otherwise a new menu is created to hold the submenu and added to the current menu set."
  :type 'boolean
  :group 'one-key)

(defcustom one-key-major-mode-remap-alist '((Custom-mode . "custom-mode")
                                            (latex-mode ."LaTeX-mode"))
  "A list of cons cells mapping major modes to one-key-menus.
The car of each cell is the symbol of a major mode function (e.g. 'emacs-lisp-mode), and the cdr is the name of
a `one-key' menu associated with the major mode.
When a menu of type \"major-mode\" is opened this alist is checked, and if the current major mode is listed then the
associated menu will be used, otherwise the menu alist with name one-key-menu-???-alist (where ??? is the name of the
current major mode) will be used (and created if necessary)."
  :type '(alist :key-type (function :tag "Major mode" :help-echo "A major mode function") :value-type (string :tag "Name of associated menu" :help-echo "The name of the menu to be associated with the major mode"))
  :group 'one-key)

(defcustom one-key-menu-toplevel-alist '((("M" . "Cursor motion commands") .
                                     (lambda nil (interactive)
                                       (one-key-open-submenu "Cursor motion commands"
                                                             one-key-menu-cursor-motion-commands-alist)))
                                    (("B" . "Buffer and file commands") .
                                     (lambda nil (interactive)
                                       (one-key-open-submenu "Buffer and file commands"
                                                             one-key-menu-buffer-and-file-commands-alist)))
                                    (("E" . "Editing commands") .
                                     (lambda nil (interactive)
                                       (one-key-open-submenu "Editing commands"
                                                             one-key-menu-editing-commands-alist)))
                                    (("C-s" . "Searching commands") .
                                     (lambda nil (interactive)
                                       (one-key-open-submenu "Searching commands"
                                                             one-key-menu-searching-commands-alist)))
                                    (("S" . "Sorting commands") .
                                     (lambda nil (interactive)
                                       (one-key-open-submenu "Sorting commands"
                                                             one-key-menu-sorting-commands-alist)))
                                    (("W" . "Window commands") .
                                     (lambda nil (interactive)
                                       (one-key-open-submenu "Window commands"
                                                             one-key-menu-window-commands-alist)))
                                    (("C-h" . "Prefix-Key:C-h (help commands)") .
                                     (lambda nil (interactive)
                                       (funcall 'one-key-prefix-key-menu-command "C-h" t)))
                                    (("<C-escape>" . "Prefix-Key:ESC (all meta key keybindings)") .
                                     (lambda nil (interactive)
                                       (funcall 'one-key-prefix-key-menu-command "ESC" t)))
                                    (("M-g" . "Prefix-Key:M-g (error commands)") .
                                     (lambda nil (interactive)
                                       (funcall 'one-key-prefix-key-menu-command "M-g" t)))
                                    (("M-o" . "Prefix-Key:M-o (font-lock/centering commands)") .
                                     (lambda nil (interactive)
                                       (funcall 'one-key-prefix-key-menu-command "M-o" t)))
                                    (("M-s" . "Prefix-Key:M-s (occur/highlight commands)") .
                                     (lambda nil (interactive)
                                       (funcall 'one-key-prefix-key-menu-command "M-s" t)))
                                    (("C-x" . "Prefix-Key:C-x (all C-x keybindings)") .
                                     (lambda nil (interactive)
                                       (funcall 'one-key-prefix-key-menu-command "C-x" t)))
                                    (("r" . "Prefix-Key:C-x r (bookmark, rectangle and register commands)") .
                                     (lambda nil (interactive)
                                       (funcall 'one-key-prefix-key-menu-command "C-x r" t)))
                                    (("v" . "Prefix-Key:C-x v (version control commands)") .
                                     (lambda nil (interactive)
                                       (funcall 'one-key-prefix-key-menu-command "C-x v" t)))
                                    (("a" . "Prefix-Key:C-x a (abbrev commands)") .
                                     (lambda nil (interactive)
                                       (funcall 'one-key-prefix-key-menu-command "C-x a" t)))
                                    (("n" . "Prefix-Key:C-x n (narrow/widen commands)") .
                                     (lambda nil (interactive)
                                       (funcall 'one-key-prefix-key-menu-command "C-x n" t)))
                                    (("C-k" . "Prefix-Key:C-x C-k (keyboard macro commands)") .
                                     (lambda nil (interactive)
                                       (funcall 'one-key-prefix-key-menu-command "C-x C-k" t)))
                                    (("w" . "Prefix-Key:C-x w (highlight commands)") .
                                     (lambda nil (interactive)
                                       (funcall 'one-key-prefix-key-menu-command "C-x w" t)))
                                    (("RET" . "Prefix-Key:C-x RET (input/coding commands)") .
                                     (lambda nil (interactive)
                                       (funcall 'one-key-prefix-key-menu-command "C-x RET" t)))
                                    (("4" . "Prefix-Key:C-x 4 (other-window commands)") .
                                     (lambda nil (interactive)
                                       (funcall 'one-key-prefix-key-menu-command "C-x 4" t)))
                                    (("5" . "Prefix-Key:C-x 5 (other-frame commands)") .
                                     (lambda nil (interactive)
                                       (funcall 'one-key-prefix-key-menu-command "C-x 5" t)))
                                    (("6" . "Prefix-Key:C-x 6 (2 column mode commands)") .
                                     (lambda nil (interactive)
                                       (funcall 'one-key-prefix-key-menu-command "C-x 6" t)))
                                    (("C-c" . "Prefix-Key:C-c (mode specific bindings)") .
                                     (lambda nil (interactive)
                                       (funcall 'one-key-prefix-key-menu-command "C-c" t)))
                                    (("@" . "Prefix-Key:C-c @ (outline commands)") .
                                     (lambda nil (interactive)
                                       (funcall 'one-key-prefix-key-menu-command "C-c @" t)))
                                    (("," . "Prefix-Key:C-c , (senator/semantic commands)") .
                                     (lambda nil (interactive)
                                       (funcall 'one-key-prefix-key-menu-command "C-c ," t)))
                                    (("." . "Prefix-Key:C-c . (Ede commands)") .
                                     (lambda nil (interactive)
                                       (funcall 'one-key-prefix-key-menu-command "C-c ." t)))
                                    (("/" . "Prefix-Key:C-c / (Srecode commands)") .
                                     (lambda nil (interactive)
                                       (funcall 'one-key-prefix-key-menu-command "C-c /" t)))
                                    )
  "The `one-key' top-level alist.
Contains list of key items for toplevel one-key menu.
Each item contains a key, description and command, in that order.
The key should be entered in the same format as that returned by `describe-key'."
  :type '(alist :key-type (cons string string) :value-type function)
  :group 'one-key)

(defcustom one-key-sets-of-menus-alist (list '("Major mode, top-level & menu sets" "major-mode" "top-level" "menu-sets"))
  "Saved menu sets (sets of menus).
Each element in this list is a cons cell whose car is a name or description for the set, and whose cdr is a list of names
of menus which make up the set. Each menu name must correspond to a type in `one-key-types-of-menu' (which see),
and `one-key' must be able to reconstruct the menu from the name (which it will be able to if the corresponding entry
in `one-key-types-of-menu' is complete.
These menu sets may be opened from the `one-key-menu-set' menu, and you may want to create different sets for different
projects."
  :type '(alist :key-type (string :tag "Set description/name" :help-echo "A name or description for this collection of menus")
                :value-type (repeat (string :tag "Menu" :help-echo "The name of the menu. Must correspond to a type in `one-key-types-of-menu'.")))
  :group 'one-key)

(defcustom one-key-default-menu-set "Major mode, top-level & menu sets"
  "The default menu set. It's value should be the car of one of the items in `one-key-sets-of-menus-alist'.
It may be changed by the user from the menu-sets `one-key' menu.
This is only meaningful if it is used with `one-key-open-menu-set' bound to a key so that the key can open a different
menu set if the user has altered its value."
  :type 'string
  :group 'one-key)

(defcustom one-key-default-sort-method-alist
  '((key . (lambda (a b) (string< (caar a) (caar b))))
    (description . (lambda (a b) (string< (cdar a) (cdar b))))
    (command . (lambda (a b) (string< (prin1-to-string (cdr a))
                                      (prin1-to-string (cdr b)))))
    (colour_name . (lambda (a b) (string< (cadr (get-text-property 0 'face (cdar a)))
                                          (cadr (get-text-property 0 'face (cdar b))))))
    (colour_hue . (lambda (a b)
                    (let* ((bg (cdr (assq 'background-color (frame-parameters))))
                           (cola (or (cadr (get-text-property 0 'face (cdar a))) bg))
                           (colb (or (cadr (get-text-property 0 'face (cdar b))) bg))
                           (hsva (destructuring-bind (r g b) (color-values cola)
                                   (color-rgb-to-hsv r g b)))
                           (hsvb (destructuring-bind (r g b) (color-values colb)
                                   (color-rgb-to-hsv r g b))))
                      (> (first hsva) (first hsvb)))))
    (colour_brightness . (lambda (a b)
                           (let* ((bg (cdr (assq 'background-color (frame-parameters))))
                                  (cola
                                   (or (cadr (get-text-property 0 'face (cdar a))) bg))
                                  (colb
                                   (or (cadr (get-text-property 0 'face (cdar b))) bg))
                                  (hsva (destructuring-bind (r g b)
                                            (color-values cola)
                                          (color-rgb-to-hsv r g b)))
                                  (hsvb (destructuring-bind (r g b)
                                            (color-values colb)
                                          (color-rgb-to-hsv r g b))))
                             (> (third hsva) (third hsvb)))))
    (colour_saturation . (lambda (a b)
                           (let* ((bg (cdr (assq 'background-color (frame-parameters))))
                                  (cola
                                   (or (cadr (get-text-property 0 'face (cdar a))) bg))
                                  (colb
                                   (or (cadr (get-text-property 0 'face (cdar b))) bg))
                                  (hsva (destructuring-bind (r g b)
                                            (color-values cola)
                                          (color-rgb-to-hsv r g b)))
                                  (hsvb (destructuring-bind (r g b)
                                            (color-values colb)
                                          (color-rgb-to-hsv r g b))))
                             (> (second hsva) (second hsvb)))))
    (length . (lambda (a b) (> (length (cdar a)) (length (cdar b))))))
  "An alist of sorting methods to use on the `one-key' menu items.
Each element is a cons cell of the form (NAME . PREDICATE) where NAME is a symbol for the name of the sort method,
and PREDICATE is a function which takes two items from the `one-key' menu alist as arguments and returns non-nil if
the first item should come before the second in the menu."
  :type '(alist :key-type (symbol :help-echo "Name for sort method (a symbol)")
                :value-type (function :help-echo "Predicate that returns non-nil if 1st item comes before 2nd"))
  :group 'one-key)

(defcustom one-key-special-keybindings
  '((quit-close "ESC" "Quit and close menu window" (lambda nil (keyboard-quit) nil))
    (quit-open"C-ESC" "Quit, but keep menu window open"
              (lambda nil (setq keep-window-p t) nil))
    (toggle-persistence "<C-menu>" "Toggle menu persistence"
                        (lambda nil (if match-recursion-p
                                        (setq match-recursion-p nil
                                              miss-match-recursion-p nil)
                                      (setq match-recursion-p t
                                            miss-match-recursion-p t))))
    (toggle-display "<menu>" "Toggle menu display" (lambda nil (one-key-menu-window-toggle) t))
    (next-menu "<left>" "Change to next menu"
               (lambda nil (if menu-number
                               (progn
                                 (setq menu-number
                                       (if (equal menu-number 0)
                                           (1- (length info-alists))
                                         (1- menu-number)))
                                 (setq one-key-menu-call-first-time t))) t))
    (prev-menu "<right>" "Change to previous menu"
               (lambda nil (if menu-number
                               (progn
                                 (setq menu-number
                                       (if (equal menu-number (1- (length info-alists)))
                                           0 (1+ menu-number)))
                                 (setq one-key-menu-call-first-time t))) t))
    (up "<up>" "Scroll/move up one line" (lambda nil (one-key-scroll-or-move-up info-alist full-list) t))
    (down "<down>" "Scroll/move down one line" (lambda nil (one-key-scroll-or-move-up info-alist full-list t) t))
    (scroll-down "<prior>" "Scroll menu down one page" (lambda nil (one-key-menu-window-scroll-up t) t))
    (scroll-up "<next>" "Scroll menu up one page" (lambda nil (one-key-menu-window-scroll-up) t))
    (help "C-h" "Show help for next item chosen"
          (lambda nil
            (let ((key (read-event "Enter the key for the item that you want help on")))
              (one-key-show-item-help key full-list)
              (setq match-recursion-p t)) t))
    (save-menu "C-s" "Save current state of menu"
               (lambda nil (one-key-save-menu this-name info-alist full-list) t))
    (toggle-help "<f1>" "Toggle this help buffer"
                 (lambda nil (if (get-buffer-window (help-buffer))
                                 (kill-buffer (help-buffer))
                               (one-key-show-help special-keybindings)) t))
    (toggle-row/column-order "<f2>" "Toggle column/row ordering of items"
                             (lambda nil (if one-key-column-major-order
                                             (setq one-key-column-major-order nil)
                                           (setq one-key-column-major-order t))
                               (setq one-key-menu-call-first-time t) t))
    (sort-next "<f3>" "Sort items by next method"
               (lambda nil (setq one-key-current-sort-method
                                 (one-key-sort-items-by-next-method info-alists full-list menu-number)) t))
    (sort-prev "<C-f3>" "Sort items by previous method"
               (lambda nil (setq one-key-current-sort-method
                                 (one-key-sort-items-by-next-method info-alists full-list menu-number t)) t))
    (reverse-order "<f4>" "Reverse order of items"
                   (lambda nil (one-key-reverse-item-order info-alists full-list menu-number) t))
    (limit-items "/" "Limit items to those matching regexp"
                 (lambda nil (setq filter-regex (read-regexp "Regular expression"))
                   (setq one-key-menu-call-first-time t) t))
    (highlight-items "C-/" "Highlight items matching regexp"
                     (lambda nil (let ((regex (read-regexp "Regular expression"))
                                       (bgcolour (read-color "Colour: ")))
                                   (one-key-highlight-matching-items
                                    info-alist full-list bgcolour
                                    (lambda (item) (string-match regex (cdar item))))) t))
    (edit-item "<f5>" "Edit a menu item"
               (lambda nil (one-key-edit-menu-item info-alist full-list) t))
    (delete-item "<f6>" "Delete a menu item"
                 (lambda nil (one-key-delete-menu-item info-alist full-list) t))
    (kill-items "<f7>" "Copy/kill coloured items"
               (lambda nil (one-key-copy/kill-items info-alist full-list filtered-list)
                 (setq one-key-menu-call-first-time t) t))
    (yank-items "<C-f7>" "Yank copied items"
                (lambda nil (one-key-yank-items info-alist full-list filtered-list)
                 (setq one-key-menu-call-first-time t) t))
    (swap-keys "<f8>" "Swap menu item keys"
               (lambda nil (one-key-swap-menu-items info-alist full-list) t))
    (add-item "<f9>" "Add a menu item"
              (lambda nil (one-key-prompt-to-add-menu-item info-alist full-list) t))
    (add-menu "<C-f9>" "Add a menu"
              (lambda nil (one-key-add-menus)
                (setq one-key-menu-call-first-time t)
                (one-key-handle-last nil self t)
                nil)) ; no need to return t since `one-key-add-menus' does recursion itself
    (remove-menu "<C-S-f9>" "Remove this menu"
                 (lambda nil (one-key-delete-menu) t))
    (move-item "<f10>" "Reposition item (with arrow keys)"
               (lambda nil (let ((key (one-key-key-description
                                       (read-event "Enter key of item to be moved"))))
                             (setq one-key-current-item-being-moved key)
                             (setq one-key-menu-call-first-time t)) t))
    (donate "<f11>" "Donate to support further development"
            (lambda nil (browse-url "http://onekeydonate.dynalias.net")))
    (report-bug "<C-f11>" "Report a bug" one-key-submit-bug-report)
    (show-menusets "C-h" "Show menus in menu set"
                   (lambda nil
                     (let* ((key (read-event "Enter the key for the menu set"))
                            (item (one-key-get-menu-item key full-list))
                            (menuset (assoc (cdar item) one-key-sets-of-menus-alist))
                            (desc (car menuset))
                            (names (cdr menuset)))
                       (message "%S" names) t)))
    (customize-menusets "C-s" "Customize menu sets"
                        (lambda nil
                          (setq one-key-menu-window-configuration nil)
                          (with-selected-window (previous-window)
                            (customize-variable 'one-key-sets-of-menus-alist)) nil))
    (change-default-menuset "<f5>" "Change default menu set"
                            (lambda nil
                              (let* ((key (read-event "Press the key of item to set as default"))
                                     (item (one-key-get-menu-item key full-list))
                                     (name (cdar item))
                                     (pos (position "menu-sets" names :test 'equal)))
                                (if name (eval `(customize-save-variable 'one-key-default-menu-set
                                                                         ,(substring-no-properties name))))
                                (if pos (setf (nth pos info-alists) (one-key-build-menu-sets-menu-alist))
                                  (setq info-alists (one-key-build-menu-sets-menu-alist))))
                              (setq one-key-menu-call-first-time t)
                              (one-key-menu-window-close) t)))
  "An list of special keys; labels, keybindings, descriptions and associated functions.
Each item in the list contains (in this order):

  1) A symbol to reference the keybinding in the special keybinding sets for different menu types.

  2) A string representation of the key (as returned by `one-key-key-description'), or a symbol referencing
     another item whose key description should be used instead (this allows you to keep you special keybindings
     in sync when you use different items for different menu types).
     Warning: make sure you don't end up with a circular set of key references or one-key will get stuck in a loop.

  3) A short description of the associated action. This description will be displayed in the one-key help buffer.

  4) A function for performing the action. The function takes no arguments but may use dynamic binding to
     read and change some of the values in the initial `one-key-menu' function call.
     The function should return t to display the `one-key' menu again after the function has finished,
     or nil to close the menu.

These keybindings may be referred to by other variables that contain the special keybindings for different one-key menu
types. See `one-key-default-special-keybindings' for example."
  :group 'one-key
  :type '(repeat (list (symbol :tag "Name" :help-echo "A reference name for this keybinding (no spaces).")
                       (string :tag "Keybinding" :help-echo "String representation of the keybinding for this action")
                       (string :tag "Description" :help-echo "Description to display in help buffer")
                       (function :tag "Function" :help-echo "Function for performing action. See description below for further details."))))

(defcustom one-key-default-special-keybindings
  '(quit-close quit-open toggle-persistence toggle-display next-menu prev-menu up down scroll-down scroll-up help
               save-menu toggle-help toggle-row/column-order sort-next sort-prev reverse-order limit-items highlight-items
               edit-item delete-item kill-items yank-items swap-keys add-item add-menu remove-menu move-item
               donate report-bug)
  "List of special keys to be used if no other set of special keys is defined for a given one-key menu type.
These keys are for performing general tasks on the menu such as sorting items, deleting items, etc.
Each element of this list is a reference to one of the keybindings defined in `one-key-special-keybindings'.
The keys will be displayed in the one-key help buffer in the order shown when the `one-key-show-help' function is executed."
  :group 'one-key
  :type '(repeat (symbol :tag "Name" :help-echo "The name/symbol corresponding to the keybinding.")))

(defcustom one-key-menu-sets-special-keybindings
  '(quit-close quit-open toggle-persistence toggle-display next-menu prev-menu up down scroll-down scroll-up show-menusets
               customize-menusets toggle-help toggle-row/column-order sort-next sort-prev reverse-order limit-items
               highlight-items change-default-menuset add-menu remove-menu donate report-bug)
  "List of special keys to be used for menu-sets menus (see `one-key-default-special-keybindings' for more info)."
  :group 'one-key
  :type '(repeat (symbol :tag "Name" :help-echo "The name/symbol corresponding to the keybinding.")))

(defun one-key-assq-list (symlist alist)
  "Return a list of the cdr's of elements of ALIST whose car's match a symbol in SYMLIST.
The matching is performed with assq so that only the first element of alist matching a symbol in SYMLIST is returned.
The elements returned will be in the same order as the elements of SYMLIST."
  (mapcar 'cdr (loop for symbol in symlist collect (assq symbol alist))))

(defun one-key-get-special-key-contents (specialkeys)
  "Given a symbol or list of symbols from `one-key-special-keybindings', return the corresponding contents for each symbol.
The first element of the contents of each item will be replaced by a key description string by following symbol references
in `one-key-special-keybindings'.
In other words if `one-key-special-keybindings' contains the items (symba symbb \"descriptiona\" commanda), and
 (symbb \"a\" \"descriptionb\" commandb), then (one-key-get-special-key-contents '(symba symbb)) will return '((\"a\" \"descriptiona\" commanda) (\"b\" \"descriptionb\" commandb)). Notice that symbb is replaced by \"a\" in the returned list since
this is the key description for symbb. At most 5 symbolic links will be followed before setting the key to nil."
  (let* ((symbs (if (listp specialkeys) specialkeys
                 (if (symbolp specialkeys) (list specialkeys)
                   (error "Invalid argument"))))
         (items (one-key-assq-list symbs one-key-special-keybindings)))
    (loop for (key . rest) in items
          for x = 1
          do (while (and key (symbolp key))
               (setq key (cadr (assoc key one-key-special-keybindings)))
               (if (> x 4) (setq key nil) (setq x (1+ x))))
          collect (cons key (if key rest (list "undefined key!"))))))

(defun one-key-get-special-key-descriptions (specialkeys)
  "Given a symbol or list of symbols from `one-key-special-keybindings', return the corresponding key descriptions.
This can be used to find out which special keys are used for a particular one-key menu type.
If `specialkeys' is a single symbol then a single string will be returned.
If `specialkeys' is a list then a list of strings will be returned."
  (let* ((keys (mapcar 'car (one-key-get-special-key-contents specialkeys)))
         (len (length keys)))
    (if (> len 1) keys (car keys))))

(defcustom one-key-disallowed-keymap-menu-keys (nconc '("M-TAB")
                                                      (one-key-get-special-key-descriptions
                                                       one-key-default-special-keybindings))
  "List of keys that should be excluded from one-key menus created from keymaps.
Each item in this list is a key description as returned by `one-key-key-description'."
  :group 'one-key
  :type '(repeat string))

(defcustom one-key-types-of-menu nil
  "A list of names of different types of `one-key' menu, and associated functions.
Each item in the list contains (in this order):

  1) The name for this menu type.

  2) A function which takes a string as its only argument and returns non-nil if that string corresponds to the name of
     a menu of this type, otherwise it returns nil.

  3) A function which takes the menu name as its only argument and returns a cons cell whose car is the new name or list
     of names for the menus, and whose cdr is a menu alist, a symbol whose value is a menu alist, or a list of symbols
     and/or menu alists. The number of names returned in the car should be equal to the number of menu alists/symbols
     returned in the cdr.

  4) An function that takes no arguments and returns a title string for the `one-key' menu in the same form as
     `one-key-default-title-format-string'. The function will be evaluated in the context of the `one-key-highlight-menu'
     function, and will be processed by `one-key-highlight' before display.
     You should look at the `one-key-highlight-menu' function to see which variables may be used in this format string.
     Alternatively if this item is nil then `one-key-default-title-format-string' will be used.

  5) Either a list of special keybindings in the same form as `one-key-default-special-keybindings', or a symbol
     whose value is such a list, or nil. If nil then `one-key-default-special-keybindings' will be used."
  :type '(repeat (list (string :tag "Name"
                               :help-echo "A name for this menu type.")
                       (function :tag "Condition"
                                 :help-echo "A function which returns the new menu name(s) when passed a name corresponding to this type, and returns nil otherwise.")
                       (choice (symbol :tag "Menu alist symbol(s)"
                                       :help-echo "A symbol whose value is a menu alist of keys for menus of this type, or a list of such menus.")
                               (function :tag "Menu alist function"
                                         :help-echo "A function which takes the menu name as its only argument and returns either a `one-key' menu alist of keys, or a symbol whose value is such a list, or a list of menus and/or symbols."))
                       (function :tag "Title string function"
                                 :help-echo "A function which returns a title string for `one-key' menus of this type.")
                       (choice (symbol :tag "Special keybindings symbol"
                                       :help-echo "A symbol whose value is a list of special keybindings for menus of this type")
                               (repeat :tag "Special keybindings"
                                       (list (string :tag "Keybinding"
                                                     :help-echo "String representation of the keybinding for this action")
                                             (string :tag "Description"
                                                     :help-echo "Description to display in help buffer")
                                             (function :tag "Function"
                                                       :help-echo "Function for performing action. See description below for further details."))))))
  :group 'one-key)

(defcustom one-key-persistent-menu-number t
  "If non-nil then when the default menu set is opened it will start with the same menu as when previously opened."
  :group 'one-key
  :type 'boolean)

(defcustom one-key-mode-line-message '(format "Press %s for help, %s to quit. Sorted by %s (%s first)."
                                              (cadr (assoc 'toggle-help one-key-special-keybindings))
                                              (cadr (assoc 'quit-close one-key-special-keybindings))
                                              one-key-current-sort-method (if one-key-column-major-order "columns" "rows"))
  "Form that when evaluated should produce a string for the mode-line in the *One-Key* buffer.
This should probably be left alone unless you remove `toggle-help' or `quit-close' from `one-key-special-keybindings'"
  :type 'sexp
  :group 'one-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Faces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defface one-key-name
  '((t (:foreground "Gold")))
  "Face for highlighting name."
  :group 'one-key)

(defface one-key-keystroke
  '((t (:foreground "DarkRed")))
  "Face for highlighting keystroke."
  :group 'one-key)

(defface one-key-prompt
  '((t (:foreground "khaki3")))
  "Face for highlighting prompt."
  :group 'one-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Global Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar one-key-copied-items nil
  "List of menu items that have been killed using the `one-key-copy/kill-items' function.")

(defvar one-key-null-keys (regexp-opt '("<remap>" "mouse" "<follow-link>"))
  "Regular expression matching key descriptions of keymap items that should be excluded from `one-key' menus.")

(defvar one-key-menu-window-configuration nil
  "Variable that records the window configuration that was in place before the popup menu window was opened.")

(defvar one-key-menu-call-first-time t
  "t if `one-key-menu' has been called non-recursively.
Set this to t if you want the menu to be redisplayed after pressing a special keybinding.")

(defvar one-key-menu-show-key-help nil
  "If true show help for function associated with next keystroke, when it is pressed in the one-key-menu.")

(defvar one-key-current-sort-method nil
  "The current method used to sort the items in the `one-key' menu list")

(defvar one-key-current-item-being-moved nil
  "The key corresponding to the item currently being moved in the `one-key' menu, or nil if none is being moved.")

(defvar one-key-current-window-state nil
  "The current state of the `one-key' window.
If nil then the window is closed, if t then it is open at normal size, otherwise is should be a string
containing the name of the buffer that was displayed when the one-key menu window was opened.")

(defvar one-key-altered-menus nil
  "List of menu alist variables that should be saved on exit if `one-key-autosave-menus' is true.")

(defvar one-key-default-menu-number nil
  "The default menu number to use when opening the default menu set if `one-key-persistent-menu-number' is non-nil.")

(defvar one-key-mode-line-format
  '("%e" "%e"
    #("-" 0 1
      (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
    mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification
    #(" " 0 1
      (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
    mode-line-position
    #(" " 0 1
      (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
    (:eval (eval one-key-mode-line-message))
    (global-mode-string
     ("" global-mode-string
      #(" " 0 1
        (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))))
    (:eval
     (unless
         (display-graphic-p)
       #("-%-" 0 3
         (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display")))))
  "The `mode-line-format' for the *One-Key* buffer.")

(defvar one-key-default-title-func (lambda nil
                                     (let* ((keystr (one-key-get-special-key-descriptions 'donate))
                                            (msg (concat "If you find this useful please press " keystr " to support the author and further development.\n"))
                                            (len (length msg))
                                            (dif (- (window-width) len))
                                            (spc (make-string (/ dif 2) ? )))
                                       (if (> dif 0)
                                           (concat spc msg)
                                         msg)))
  "Default message for one-key menus, prompting for donations.")

(defvar one-key-maintainer-email "vapniks@yahoo.com"
  "Email address of current maintainer.")

(defvar one-key-version "1.0"
  "Version number of this version of one-key")

;; some menus for the toplevel
(defvar one-key-menu-sorting-commands-alist
  '((("l" . "Sort lines lexicographically (M-x sort-lines)") . sort-lines)
    (("p" . "Sort paragraphs lexicographically (M-x sort-paragraphs)") . sort-paragraphs)
    (("P" . "Sort pages lexicographically (M-x sort-pages)") . sort-pages)
    (("f" . "Sort lines lexicographically by whitespace seperated fields (M-x sort-fields)") . sort-fields)
    (("n" . "Sort lines numerically by whitespace seperated fields (M-x sort-numeric-fields)") . sort-numeric-fields)
    (("c" . "Sort lines lexicographically by columns defined by point and mark (M-x sort-columns)") . sort-columns)
    (("r" . "Sort lines matching 1st regexp by field matching 2nd regexp, lexicographically (M-x sort-regexp-fields)") . sort-regexp-fields)
    (("R" . "Reverse order of lines in region (M-x reverse-region)") . reverse-region))
  "The `one-key' menu alist for sorting commands.")

(defvar one-key-menu-searching-commands-alist
  '((("C-s" . "Search forward (C-s)") . (lambda nil (interactive) (isearch-mode t nil nil t)))
    (("C-r" . "Search backward (C-r)") . (lambda nil (interactive) (isearch-mode nil nil nil t)))
    (("M-%" . "Replace string (M-%)") . query-replace)
    (("C-M-s" . "Search forward for regexp (C-M-s)") . isearch-forward-regexp)
    (("C-M-r" . "Search backward for regexp (C-M-r)") . isearch-backward-regexp)    
    (("C-M-%" . "Replace regexp (C-M-%)") . query-replace-regexp)
    (("b" . "Regular expression builder (M-x re-builder)") . re-builder))
  "The `one-key' menu alist for search/replace commands.")

(defvar one-key-menu-editing-commands-alist
  '((("C-SPC" . "Set mark (C-SPC)") . set-mark-command)
    (("h" . "Mark entire buffer (C-x h)") . mark-whole-buffer)
    (("C-p" . "Mark page (C-x C-p)") . mark-page)
    (("h" . "Mark paragraph (M-h)") . mark-paragraph)
    (("C-x" . "Exchange point and mark (C-x C-x)") . exchange-point-and-mark)
    (("M-w" . "Copy selected region (M-w, C-Insert)") . kill-ring-save)
    (("C-w" . "Cut selected region (C-w, S-DEL)") . kill-region)
    (("C-y" . "Paste/yank last copied/cut text (C-y)") . yank)
    (("M-y" . "Paste/yank ealier copied/cut text (M-y)") . yank-pop)
    (("C-d" . "Delete character forward (C-d)") . delete-char)
    (("M-DEL" . "Delete word backward (M-DEL)") . backward-kill-word)
    (("M-D" . "Delete word forward (M-D, C-S-DEL)") . kill-word)
    (("C-k" . "Delete to end of line (C-k)") . kill-line)
    (("M-k" . "Delete to end of sentence (M-k)") . kill-sentence)
    (("M-u" . "Make word uppercase (M-u)") . upcase-word)
    (("M-l" . "Make word lowercase (M-l)") . downcase-word)
    (("M-c" . "Capitalize word (M-c)") . capitalize-word)
    (("C-t" . "Transpose/swap adjacent characters (C-t)") . transpose-chars)
    (("M-t" . "Transpose/swap adjacent words (M-t)") . transpose-words)
    (("t" . "Transpose/swap lines (C-x C-t)") . transpose-lines)
    (("M-^" . "Join previous line (M-^)") . delete-indentation)
    (("TAB" . "Indent all lines in region (C-x TAB)") . indent-rigidly)
    (("M-q" . "Fill or justify paragraph (M-q)") . fill-paragraph)
    (("C-o" . "Open a new line before this one (C-o)") . open-line)
    (("1" . "Remove all but 1 empty line (C-x C-o)") . delete-blank-lines))
  "The `one-key' menu alist for editing commands.")

(defvar one-key-menu-cursor-motion-commands-alist 
  '((("C-f" . "Forward a character (C-f)") . forward-char)
    (("C-b" . "Backward a charcter (C-b)") . backward-char)
    (("C-n" . "Forward a line (C-n)") . next-line)
    (("C-p" . "Backward a line (C-p)") . previous-line)
    (("C-a" . "Goto the beginning of line (C-a)") . move-beginning-of-line)
    (("C-e" . "Goto the end of line (C-e)") . move-end-of-line)
    (("M-f" . "Forward a word (M-f)") . forward-word)
    (("M-b" . "Backward a word (M-b)") . backward-word)
    (("M-e" . "Forward a sentence (M-e)") . forward-sentence)
    (("M-a" . "Backward a sentence (M-a)") . backward-sentence)
    (("M-<" . "Goto the beginning of buffer (M-<)") . beginning-of-buffer)
    (("M->" . "Goto the end of buffer (M->)") . end-of-buffer)
    (("{" . "Move backwards paragraph (M-{") . backward-paragraph)
    (("}" . "Move forwards paragraph (M-})") . forward-paragraph)
    (("[" . "Move backwards page (C-x [)") . backward-page)
    (("]" . "Move forwards page (C-x ])") . forward-page))
  "The `one-key' menu alist for cursor motion commands.")

(defvar one-key-menu-buffer-and-file-commands-alist 
  '((("C-f" . "Open file (C-x C-f)") . find-file)
    (("d" . "Open directory (C-x d)") . (lambda nil (interactive)
                                          (if (featurep 'ido) (call-interactively 'ido-dired)
                                            (call-interactively 'dired))))
    (("C-s" . "Save current file (C-x C-s)") . save-buffer)
    (("C-w" . "Save file as (C-x C-w)") . write-file)
    (("s" . "Prompt user for files to save (C-x s)") . save-some-buffers)
    (("i" . "Insert file contents into buffer (C-x i)") . insert-file)
    (("b" . "Switch to another buffer (C-x b)") . switch-to-buffer)
    (("4" . "Switch to another buffer in another window (C-x 4 b)") . switch-to-buffer-other-window)
    (("5" . "Switch to another buffer in another frame (C-x 5 b)") . switch-to-buffer-other-frame)
    (("<C-left>" . "Select previous buffer (C-x <left>)") . previous-buffer)
    (("<C-right>" . "Select next buffer (C-x <right>)") . next-buffer)    
    (("k" . "Kill buffer (C-x k)") . kill-buffer)
    (("K" . "Prompt to kill buffers (M-x kill-some-buffers)") . kill-some-buffers)
    (("C-v" . "Kill current buffer then open another file (C-x C-v)") . find-alternate-file)
    (("m" . "Menu for switching/killing buffers (M-x bs-show)") . bs-show)
    (("l" . "List existing buffers (C-x C-b)") . list-buffers)
    (("C-q" . "Toggle read-only status of buffer (C-x C-q)") . toggle-read-only)
    (("R" . "Rename buffer (M-x rename-buffer)") . rename-buffer)
    )
  "The `one-key' menu alist for buffer and file commands.")

(defvar one-key-menu-window-commands-alist
  '((("1" . "Make this window fill it's frame (C-x 1)") . delete-other-windows)
    (("2" . "Split this window vertically (C-x 2)") . split-window-vertically)
    (("3" . "Split this window horizontally (C-x 3)") . split-window-horizontally)
    (("o" . "Select next window (C-x o)") . other-window)
    (("0" . "Kill this buffer and window (C-x 4 0)") . kill-buffer-and-window)
    (("^" . "Make this window taller (C-x ^)") . enlarge-window)
    (("-" . "Make this window shorter (C-x -)") . shrink-window-if-larger-than-buffer)
    (("}" . "Make this window wider (C-x })") . enlarge-window-horizontally)
    (("{" . "Make this window narrower (C-x {)") . shrink-window-horizontally)
    (("+" . "Make all windows the same height (C-x +)") . balance-windows)
    (("C-M-v" . "Scroll the next window (C-M-v)") . scroll-other-window)
    (("b" . "Select buffer in another window (C-x 4 b)") . switch-to-buffer-other-window)
    (("C-o". "Display buffer in another window (C-x 4 C-o)") . display-buffer)
    (("f" . "Open a file in another window (C-x 4 f)") . find-file-other-window)
    (("d" . "Open a directory in another window (C-x 4 d)") . dired-other-window)
    (("m" . "Compose email in another window (C-x 4 m)") . mail-other-window)
    (("." . "Find tag of current tags table in another window (C-x 4 .)") . find-tag-other-window)
    (("r" . "Open a file as read-only in another window (C-x 4 r)") . find-file-read-only-other-window))
  "The `one-key' menu alist for window commands.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun one-key-show-help (special-keybindings)
  "Show information about `one-key-menu' special keybindings in the alist SPECIAL-KEYBINDINGS."
  (interactive)
  (let* ((maxkey (loop for elt in special-keybindings
                       maximize (1+ (length (car elt)))))
         (maxstr (loop for elt in special-keybindings
                       maximize (+ 3 maxkey (length (cadr elt)))))
         (width (/ (window-width) 2))
         (keystr (if (> maxstr width)
                     (mapconcat (lambda (elt) (format "%s\t: %s"
                                                      (one-key-remap-key-description (car elt))
                                                      (cadr elt)))
                                special-keybindings "\n")
                   (loop with colsize = (+ (/ (length special-keybindings) 2)
                                           (% (length special-keybindings) 2))
                         with finalstr
                         for n from 0 to (1- colsize)
                         for (key1 desc1) = (nth n special-keybindings)
                         for (key2 desc2) = (nth (+ n colsize) special-keybindings)
                         for key1a = (one-key-remap-key-description key1)
                         for key2a = (one-key-remap-key-description key2)
                         for keyspc1 = (make-string (- maxkey (length key1a)) ? )
                         for keyspc2 = (make-string (- maxkey (length key2a)) ? )
                         for str1 = (format "%s%s: %s" key1a keyspc1 desc1)
                         for str2 = (format "%s%s: %s" key2a keyspc2 desc2)
                         for spc = (make-string (- width (length str1)) ? ) do
                         (push (concat str1 spc (if key2a str2) "\n") finalstr)
                         finally return (mapconcat 'identity (nreverse finalstr) "")))))
    (with-help-window (help-buffer)
      (princ (concat "Press the highlighted key in the menu to perform the corresponding action written next to it.
The following special keys may also be used:\n"
                     keystr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun one-key-add-to-alist (alist-var elt-cons &optional no-replace)
  "Add to the value of ALIST-VAR an element ELT-CONS if it isn't there yet, and return the new list.
If an element with the same car as the car of ELT-CONS is already present,
replace it with ELT-CONS unless NO-REPLACE is non-nil; if a matching
element is not already present, add ELT-CONS to the front of the alist.
The test for presence of the car of ELT-CONS is done with `equal'."
  (let ((existing-element (assoc (car elt-cons) (symbol-value alist-var))))
    (if existing-element
        (or no-replace
            (rplacd existing-element (cdr elt-cons)))
      (set alist-var (cons elt-cons (symbol-value alist-var)))))
  (symbol-value alist-var))

(defun one-key-add-elements-to-alist (alist-var newelts &optional no-replace)
  "Use `one-key-add-to-alist' to add each element of NEWELTS to ALIST-VAR.
NO-REPLACE has the same meaning as in `one-key-add-to-alist'."
  (loop for elt in newelts do
        (one-key-add-to-alist alist-var elt no-replace))
  (symbol-value alist-var))

(defun one-key-scroll-or-move-up (info-alist full-list &optional down)
  "Either scroll the `one-key' menu window up by one line or move an item up.
If DOWN is non-nil move down instead of up.
If `one-key-current-item-being-moved' contains a string representation of one of the keys in the menu move that item
up/down one line, otherwise scroll the window up/down one line.
FULL-LIST is as in the `one-key-menu' function."
  (let* ((key one-key-current-item-being-moved)
         (pos (position-if (lambda (x) (equal (caar x) key)) full-list)))
    (if pos
        (let* ((len (length full-list))
               (ppos (if down (1+ pos) (1- pos)))
               (prevpos (if (eq pos 0) (1- len) ppos))
               (item (nth pos full-list))
               (previtem (nth prevpos full-list))
               (copy (copy-list item)))
          (setf (car item) (car previtem) (cdr item) (cdr previtem)
                (car previtem) (car copy) (cdr previtem) (cdr copy))
          (setq one-key-menu-call-first-time t)
          (one-key-menu-window-close)
          (if (symbolp info-alist)
              (add-to-list 'one-key-altered-menus (symbol-name info-alist))))
      (one-key-menu-window-scroll-up-line down)))
  (setq protect-function (lambda nil (interactive) (setq one-key-current-item-being-moved nil))))

(defun one-key-show-item-help (key menu-alist)
  "Show help for item in MENU-ALIST that is associated with the key KEY.
MENU-ALIST should be a menu of items as used by the `one-key-menu' function."
  (let* ((item (one-key-get-menu-item key menu-alist))
         (tail (cdr item)))
    (if (listp tail)
        (if (commandp tail)
            (with-help-window (help-buffer)
              (princ tail))
          (let ((cmd (car tail)))
            (if (symbolp cmd)
                (if (commandp cmd)
                    (describe-function cmd)
                  (message "Unknown item!"))
              (with-help-window (help-buffer)
                (princ cmd)))))
      (if (commandp tail)
          (describe-function tail)
        (message "Unknown item!")))))

(defun one-key-prompt-to-add-menu-item (info-alist full-list)
  "Prompt the user for item details and add it to the current `one-key' menu.
INFO-ALIST and FULL-LIST are as in the `one-key-menu' function."
  (let* ((isref (symbolp info-alist))
         (newkey (let ((key (read-event "Enter the key for the new item")))
                   (while (and (one-key-get-menu-item key full-list)
                               (not (y-or-n-p "That key is already used! Overwrite old item?")))
                     (setq key (read-event "Enter new key for the item")))
                   key))
         (desc (read-string "Item description: "))
         (contents (read-from-minibuffer "Command: " nil nil t)))
    (if isref
        (progn (add-to-list 'one-key-altered-menus (symbol-name info-alist))
               (set info-alist (one-key-add-menu-item newkey desc contents full-list)))
      (setq info-alist (one-key-add-menu-item newkey desc contents full-list))))
    (setq one-key-menu-call-first-time t)
    (one-key-menu-window-close))

(defun one-key-swap-menu-items (info-alist full-list)
  "Prompt user for a pair of items in the `one-key' menu and swap the corresponding keys.
INFO-ALIST and FULL-LIST are as in the `one-key-menu' function."
  (let* ((keya (read-event "Press key for first item"))
         (keyastr (one-key-key-description keya))
         (itema (one-key-get-menu-item keyastr full-list))
         (keyb (read-event "Press key for second item"))
         (keybstr (one-key-key-description keyb))
         (itemb (one-key-get-menu-item keybstr full-list)))
    (if (not (and itema itemb)) (message "Invalid key!")
      (setf (caar itema) keybstr (caar itemb) keyastr))
    (if (symbolp info-alist)
        (add-to-list 'one-key-altered-menus (symbol-name info-alist)))
    (setq one-key-menu-call-first-time t)
    (one-key-menu-window-close)))

(defun one-key-delete-menu-item (info-alist full-list)
  "Prompt the user for an item to delete from the `one-key' menu.
INFO-ALIST and FULL-LIST are as in the `one-key-menu' function."
  (let* ((isref (symbolp info-alist))
         (key (read-event "Press the key of the item you want to delete"))
         (item (one-key-get-menu-item key full-list)))
    (if (and item (y-or-n-p (format "Delete item \"%s\"?" (cdar item))))
        (if isref (set info-alist (delete item full-list))
          (setq info-alist (delete item full-list))))
    (if isref (add-to-list 'one-key-altered-menus (symbol-name info-alist)))
    (setq one-key-menu-call-first-time t)
    (one-key-menu-window-close)))

(defun one-key-get-item-colour (item &optional fg rettype)
  "Return the background colour of menu item ITEM. If FG is non-nil return the foreground colour instead.
If RETTYPE is one of the following symbols: 'hex, 'hsv or 'rgb, then the colour will be returned in the associated format
 (a hex string, list of hsv values or list of rgb values).
By default the colour will be returned in hex string format."
  (let* ((descface (get-text-property 0 'face (cdar item)))
         (type (if fg :foreground :background))
         (colour (or (and (facep descface)
                          (not (equal (face-attribute descface type) 'unspecified))
                          (face-attribute descface type))
                     (plist-get descface type)))
         (colour2 (or colour (if fg one-key-item-foreground-colour
                               (cdr (assq 'background-color (frame-parameters)))))))
    (case rettype
      (hex (hexrgb-color-name-to-hex colour2))
      (hsv (hexrgb-hex-to-hsv colour2))
      (rgb (hexrgb-hex-to-rgb colour2))
      (t (hexrgb-color-name-to-hex colour2)))))

(defun one-key-copy/kill-items (info-alist full-list filtered-list)
  "Prompt for a colour, copy all items with that colour from the current menu, and put them in `one-key-copied-items'."
  (let* ((isref (symbolp info-alist))
         (key (read-event "Press the key of an item in the colour group to copy: "))
         (item (one-key-get-menu-item key filtered-list))
         (kill (y-or-n-p "Delete items from current menu?")))
    (if item
        (destructuring-bind (h1 s1 v1) (one-key-get-item-colour item nil 'hsv)
          (setq one-key-copied-items
                (loop for item2 in filtered-list
                      for (h2 s2 v2) = (one-key-get-item-colour item2 nil 'hsv)
                      for huediff = (abs (- h1 h2))
                      for satdiff = (abs (- s1 s2))
                      if (and (< huediff 0.001) (< satdiff 0.001)) do
                      (if kill
                          (if isref (set info-alist (delete item2 full-list))
                            (setq info-alist (delete item2 full-list))))
                      and collect item2))
          (if isref (add-to-list 'one-key-altered-menus (symbol-name info-alist)))))))

(defun one-key-yank-items (info-alist full-list filtered-list)
  "Yank menu items in `one-key-copied-items' into current menu."
  (let* ((isref (symbolp info-alist))
         (usedkeys (mapcar 'caar full-list))
         (pair (loop for ((key . desc) . rest) in one-key-copied-items
                     for newkey = (one-key-generate-key desc usedkeys nil key)
                     for newitem = (cons (cons newkey desc) rest)
                     do (add-to-list 'usedkeys newkey)
                     collect newitem into newitems
                     collect desc into descs
                     finally return (cons newitems descs)))
         (newitems (car pair))
         (descs (cdr pair)))
    (if isref (set info-alist (append full-list newitems))
      (setq info-alist (append full-list newitems)))
    (if filter-regex (setq filter-regex (concat (regexp-opt descs) "\\|" filter-regex)))
    (if isref (add-to-list 'one-key-altered-menus (symbol-name info-alist)))))

(defun one-key-edit-menu-item (info-alist full-list)
  "Prompt user for the key of a menu item to edit, make changes and then reopen `one-key' menu.
INFO-ALIST and FULL-LIST are as in the `one-key-menu' function."
  (let* ((oldkey (read-event "Press the key of the item you want to edit"))
         (item (one-key-get-menu-item oldkey full-list))
         (newkey (let ((key (read-event "Enter new key for the item")))
                   (while (and (one-key-get-menu-item key full-list)
                               (not (eq key oldkey))
                               (not (y-or-n-p "That key is already used! Use it anyway?")))
                     (setq key (read-event "Enter new key for the item")))
                   key))
         (desc (read-string "Item description: " (cdar item) nil nil))
         (oldcontents (cdr item))
         (contents (read-from-minibuffer "Item contents: " (format "%S" oldcontents) nil t)))
    (setf (caar item) (one-key-key-description newkey))
    (setf (cdar item) desc)
    (setf (cdr item) contents))
  (if (symbolp info-alist)
      (add-to-list 'one-key-altered-menus (symbol-name info-alist)))
  (setq one-key-menu-call-first-time t)
  (one-key-menu-window-close))

(defun one-key-highlight-matching-items (info-alist full-list colour pred)
  "Highlight items in FULL-LIST with colour COLOUR using predicate function PRED to select items.
The predicate function should take a single item from FULL-LIST as it's only argument.
INFO-ALIST and FULL-LIST are as in the `one-key-menu' function.
If COLOUR is \"\" then all highlighting (and more generally any text properties) are removed from the item."
  (loop for item in-ref full-list
        for str = (cdar item)
        if (funcall pred item) do
        (if (equal colour "")
            (setf (cdar item) (substring-no-properties str))
          (setf (cdar item)
                (propertize str 'face (list :background colour :foreground one-key-item-foreground-colour)))))
  (if (symbolp info-alist)
      (add-to-list 'one-key-altered-menus (symbol-name info-alist)))
  (setq one-key-menu-call-first-time t)
  (one-key-menu-window-close))

(defun one-key-save-menu (name info-alist full-list)
  "Save a `one-key' menu to the file `one-key-menus-save-file'.
NAME is the name of the menu, INFO-ALIST and FULL-LIST are as in the `one-key-menu' function."
  (let* ((varname (if (symbolp info-alist) (symbol-name info-alist)
                    (concat "one-key-menu-" name "-alist")))
         (file one-key-menus-save-file)
         (buf (get-file-buffer file)))
    (if file
        (if (file-writable-p file)
                (with-current-buffer (find-file-noselect file)
                  (goto-char (point-min))
                  (if (not (search-forward (concat "(setq " varname) nil t))
                      (goto-char (point-max))
                    (beginning-of-line)
                    (mark-sexp)
                    (kill-region (point) (marker-position (mark-marker)))
                    (deactivate-mark))
                  (insert (concat "(setq " varname "\n      '"
                                  (replace-regexp-in-string
                                   ") ((" ")\n        ((" (eval `(prin1-to-string full-list))) ")"))
                  (save-buffer)
                  (if (not buf) (kill-buffer (get-file-buffer file))))
              (message "Can't write to file %s" file))
      (message "`one-key-menus-save-file' not set" file))))

(defun one-key-get-next-alist-item (currentcar allitems-alist &optional prev)
  "Get the item in ALLITEMS-ALIST which comes after the item with car equal to CURRENTCAR.
If CURRENTCAR is the car of the last item in ALLITEMS-ALIST, then return the first item in the list.
If PREV is non-nil then return the previous item instead of the next item (returning the last item if CURRENTCAR is
the car of the first item)."
  (let* ((pos (position currentcar allitems-alist
                        :test (lambda (a b) (equal a (car b)))))
         (len (length allitems-alist))
         (newpos (if prev (if (and pos (> pos 0))
                              (1- pos) (1- len))
                   (if (and pos (< pos (1- len)))
                       (1+ pos) 0))))
    (nth newpos allitems-alist)))

(defun* one-key-sort-items-by-next-method (info-alists full-list menu-number &optional prev)
  "Sort the items in the current `one-key' menu.
This function is called in the context of the `one-key-menu', where INFO-ALISTS, FULL-LIST and MENU-NUMBER are defined.
Sort the items in FULL-LIST according to the method in `one-key-default-sort-method-alist' that comes after `one-key-current-sort-method', or the previous method if PREV is non-nil.
Return the symbol corresponding to the sort method used."
  (let* ((info-alist (if menu-number (nth menu-number info-alists) info-alists))
         (isref (symbolp info-alist))
         (nextmethod (one-key-get-next-alist-item
                      one-key-current-sort-method
                      one-key-default-sort-method-alist prev))
         (sorted-list (sort (copy-list full-list)
                            (cdr nextmethod)))
         (major (if one-key-column-major-order "columns" "rows")))
    (if isref (progn (set info-alist sorted-list)
                     (add-to-list 'one-key-altered-menus (symbol-name info-alist)))
      (if menu-number
          (setf (nth menu-number info-alists) sorted-list)
        (setq info-alists sorted-list)))
    (setq one-key-menu-call-first-time t)
    (one-key-menu-window-close)
    (car nextmethod)))

(defun one-key-reverse-item-order (info-alists full-list menu-number)
  "Reverse the order of items in the current `one-key' menu.
This function is called in the context of the `one-key-menu', where INFO-ALISTS, FULL-LIST and MENU-NUMBER are defined.
The function will reverse the order of the items in FULL-LIST, and then update the MENU-NUMBER'th item in INFO-ALIST,
or the list that it points to (if its value is a symbol)."
  (let* ((info-alist (nth menu-number info-alists))
         (isref (symbolp info-alist))
         (reversed-list (reverse full-list)))
    (if isref (progn (set info-alist reversed-list)
                     (add-to-list 'one-key-altered-menus (symbol-name info-alist)))
      (if menu-number
          (setf (nth menu-number info-alists) reversed-list)
        (setq info-alists reversed-list)))
    (setq one-key-menu-call-first-time t)
    (one-key-menu-window-close)))

(defun one-key-get-menu-type (name)
  "Return the element of ``one-key-types-of-menu' corresponding to menu with name NAME, or nil if none exists."
  (find-if (lambda (x)
             (let ((one (first x))
                   (two (second x)))
               (or (equal one name) 
                   (and (functionp two)
                        (funcall two name)))))
           one-key-types-of-menu))

(defun one-key-get-menus-for-type (name)
  "Given the name of an existing menu or menu type in `one-key-types-of-menu', return associated names and menu alists.
If no such menu or menu type exists, return nil."
  (let* ((listname (concat "one-key-menu-" name "-alist"))
         (type (one-key-get-menu-type name))
         (func (or (third type)
                   (loop for sym being the symbols
                         for symname = (symbol-name sym)
                         when (equal listname symname)
                         return (cons name sym))
                   (error "Invalid menu name: \"%s\"" name))))
    (if (functionp func) (funcall func name) func)))

(defun one-key-prompt-for-menu nil
  "Prompt the user for a `one-key' menu type, and return menu name(s) and menu alist(s)."
  (let* ((alltypes (remove nil (mapcar 'car one-key-types-of-menu)))
         (type (if (featurep 'ido)
                   (ido-completing-read "Menu type: " alltypes)
                 (completing-read "Menu type: " alltypes))))
    (one-key-get-menus-for-type type)))
    
(defun one-key-add-menus (&optional newnames newlists)
  "Add a menu/menus to the current list of menus in the `one-key' menu function call.
This function assumes dynamic binding of the `info-alists', `menu-number' and `names' arguments to the `one-key-menu'
function, and is called within that function."
  (let* ((both (if (and newnames newlists)
                   (cons newnames newlists)
                 (one-key-prompt-for-menu)))
         (newnames (car both))
         (newlists (cdr both))
         (multi (listp newnames)))
    (if menu-number
        (let* ((listlen (length info-alists))
               (namen (length names))
               (titnum (min menu-number namen)))
          (setq names
                (concatenate 'list
                             (subseq names 0 (1+ titnum))
                             (if multi newnames (list newnames))
                             (subseq names (1+ titnum) namen))
                info-alists
                (concatenate 'list
                             (subseq info-alists 0 (1+ menu-number))
                             (if multi newlists (list newlists))
                             (subseq info-alists (1+ menu-number) listlen))
                menu-number (1+ menu-number)))
      (setq menu-number 1
            info-alists (if multi (concatenate 'list (list info-alists) newlists)
                          (list info-alists newlists))
            names (if multi (concatenate 'list (list this-name) newnames)
                    (list this-name newnames))))))

(defun* one-key-delete-menu (&optional (name this-name))
  "Remove the menu with name NAME from the list of menus, or the current menu if NAME is not supplied.
This function will only work if called within the context of the `one-key-menu' function since it depends on the dynamic
binding of the info-alists, menu-number and names variables."
  (if menu-number
      (let* ((listlen (length info-alists))
             (namen (length names))
             (pos (if name (position name names :test 'equal)
                    (min menu-number (1- namen)))))
        (setq names
              (concatenate 'list
                           (subseq names 0 pos)
                           (subseq names (1+ pos) namen))
              info-alists
              (concatenate 'list
                           (subseq info-alists 0 pos)
                           (subseq info-alists (1+ pos) listlen))
              menu-number (min pos (- listlen 2))
              this-name name
              one-key-menu-call-first-time t))
    (one-key-menu-close)))
    
(defun one-key-open-menus (names &optional menu-number protect-function)
  "Invoke `one-key-menu' with names and corresponding menu-alists.
NAMES should be the name of a single `one-key' menu or menu type, or a list of such names.
If called interactively a single name will be prompted for."
  (let* ((names (if (stringp names) (list names) names))
         (pairs (mapcar 'one-key-get-menus-for-type names))
         (names (mapcan (lambda (x) (let ((y (car x))) (if (stringp y) (list y) y))) pairs))
         (alists (mapcan (lambda (x) (let ((a (car x)) (b (cdr x)))
                                       (if (stringp a) (list b) b))) pairs)))
    (one-key-menu names alists
                  :menu-number menu-number
                  :protect-function protect-function)))

(defun one-key-open-menu-set (menuset &optional menu-number protect-function)
  "Open `one-key' menus defined by `one-key' menu set MENUSET.
MENUSET should be the car of an element of `one-key-sets-of-menus-alist'.
If called interactively, MENUSET will be prompted for."
  (interactive (list (if (featurep 'ido)
                         (ido-completing-read "Menu set: " (mapcar 'car one-key-sets-of-menus-alist))
                       (completing-read "Menu set: " (mapcar 'car one-key-sets-of-menus-alist)))))
  (let* ((item (assoc menuset one-key-sets-of-menus-alist))
         (names (cdr item)))
    (one-key-open-menus names menu-number protect-function)))

(defun one-key-open-default-menu-set nil
  "Open the menu set defined by `one-key-default-menu-set'."
  (interactive)
  (one-key-open-menu-set one-key-default-menu-set
                         (if one-key-persistent-menu-number
                             one-key-default-menu-number nil)))

(defun one-key-highlight (msg msg-regexp msg-face)
  "Highlight text in string `MSG' that matches regular expression `MSG-REGEXP' with face `MSG-FACE'."
  (with-temp-buffer
    (insert msg)
    (goto-char (point-min))
    (while (re-search-forward msg-regexp nil t)
      (add-text-properties (match-beginning 0)
                           (match-end 0)
                           msg-face))
    (buffer-string)))

(defun one-key-header-line-format (names menu-number)
  "Return the preferred value of `header-line-format' for the *One-Key* buffer.
NAMES should be the current list of menu names displayed, or just a single name if there is only one menu.
MENU-NUMBER should be nil if NAMES is a single name, otherwise it should index the current menu in NAMES."
  (let* ((prenames (if (and menu-number (> menu-number 0))
                       (concat (mapconcat 'identity (subseq names 0 menu-number) " ") " ")))
         (nameslen (length names))
         (postnames (if (and menu-number (< menu-number (1- nameslen)))
                        (concat " " (mapconcat 'identity (subseq names (1+ menu-number) nameslen) " "))))
         (name (if menu-number (nth menu-number names) names))
         (name1 (propertize name 'face 'one-key-name))
         (namelen (length name))
         (prelen (length prenames))
         (postlen (length postnames))
         (winwidth (window-width))
         (namepos (/ (- winwidth namelen) 2))
         (startpos (- namepos prelen))
         (postnames2 (if postnames (substring postnames 0 (min namepos postlen)))))
    (if (>= startpos 0)
        (concat (make-string startpos ? ) prenames name1 postnames2)
      (concat (substring prenames (- startpos) prelen) name1 postnames2))))

(defun one-key-highlight-menu (keystroke names menu-number)
  "Highlight items in KEYSTROKE (an alist of menu items), and return contents for insertion in *One-Key* buffer.
Also create header-line from NAMES (a list of menu names), highlighting the MENU-NUMBER'th name in that list.
MENU-NUMBER should be the number of the currently selected menu in the NAMES list, or nil if NAMES contains
a single menu name."
  (let* ((name (if menu-number (nth menu-number names) names))
         (title-func (or (fourth (one-key-get-menu-type name)) one-key-default-title-func))
         (infoline (if title-func (one-key-highlight (funcall title-func)
                                                     "\\(<[^<>]*>\\|'[^']*'\\)" '(face one-key-name))
                     nil))
         (keystrokelist (one-key-highlight keystroke "\\[\\([^\\[\\]\\)*?\\]" '(face one-key-keystroke))))
    (setq header-line-format (one-key-header-line-format names menu-number)
          mode-line-format one-key-mode-line-format)
    (concat infoline keystrokelist)))

(defun* one-key-menu (names
                      info-alists
                      &key
                      (menu-number
                       (if ; hack to check if info-alists is a list of lists or just a single list
                           (and (listp info-alists) (not (and (listp (car info-alists))
                                                              (listp (caar info-alists))
                                                              (stringp (caaar info-alists)))))
                           0 nil))
                      keep-window-p
                      execute-when-miss-match-p
                      miss-match-recursion-p
                      match-recursion-p
                      protect-function
                      alternate-function
                      filter-regex)
  "Function to open `one-key' menu of commands. The commands are executed by pressing the associated keys.
If global variable `one-key-popup-window' is t (default) then a menu window will be displayed showing the keybindings.
NAMES is the name of the menu as displayed in the menu window, or a list of names corresponding to different menu
lists in INFO-ALISTS.
INFO-ALISTS is either a list of menu items, a list of such lists or a symbol whose value is a list or list of lists.
Each item in a menu list is of the form: ((key . description) . command).
If INFO-ALISTS is a list then MENU-NUMBER should be an index (starting at 0) indicating which list to display
initially (default is 0), otherwise it should be nil.
The user can switch between the menu lists by pressing the appropriate keys in `one-key-default-special-keybindings'.
If KEEP-WINDOW-P is non-nil then the menu window will be kept open even after exiting.
If EXECUTE-WHEN-MISS-MATCH-P is nil then keys not matching menu items or `one-key-default-special-keybindings' will be ignored, otherwise the associated commands in the current keymaps will be executed.
The arguments MATCH-RECURSION-P and MISS-MATCH-RECURSION-P indicate whether to execute `one-key-menu' recursively after
a matching or non-matching key are pressed (and corresponding commands executed) respectively.
PROTECT-FUNCTION, if non-nil, is a function that is called within an `unwind-protect' statement at the end
of `one-key-menu'.
ALTERNATE-FUNCTION if non-nil is a function that is called after each key press while the menu is active.
If FILTER-REGEX is non-nil then only menu items whose descriptions match FILTER-REGEX will be displayed."
  (let* ((menu-number (or (and menu-number ; make sure menu number is set properly
                               (min menu-number (1- (length info-alists))))
                          (if (and (listp info-alists)
                                   (not (and (listp (car info-alists))
                                             (listp (caar info-alists))
                                             (stringp (caaar info-alists)))))
                              0 nil)))
         (info-alist (if menu-number
                         (nth menu-number info-alists)
                       info-alists))
         (issymbol (symbolp info-alist))
         (full-list (if issymbol
                        (eval info-alist)
                      info-alist))
         (this-name (if (stringp names) names
                       (nth menu-number names)))
         ;; the list of items after filtering
         (filtered-list (if (stringp filter-regex)
                            (remove-if-not
                             (lambda (elt) (string-match filter-regex (cdar elt)))
                             full-list)
                          full-list))
         ;; the special keybindings for this menu
         (special-keybindings-var (or (fifth (one-key-get-menu-type this-name))
                                      one-key-default-special-keybindings))
         (special-keybindings (one-key-get-special-key-contents (if (symbolp special-keybindings-var)
                                                                    (eval special-keybindings-var)
                                                                  special-keybindings-var)))
         ;; following function is used for recursively calling itself when needed
         (self (function (lambda () (one-key-menu names info-alists
                                                  :menu-number menu-number
                                                  :keep-window-p keep-window-p
                                                  :execute-when-miss-match-p execute-when-miss-match-p
                                                  :miss-match-recursion-p miss-match-recursion-p
                                                  :match-recursion-p match-recursion-p
                                                  :protect-function protect-function
                                                  :alternate-function alternate-function
                                                  :filter-regex filter-regex)))))
    (unwind-protect
        ;; read a key and get the key description
        (let* ((namelist (if (listp names) names nil))
               (event (read-event (if one-key-menu-call-first-time
                                    ;; just show the menu buffer when first called
                                    (progn (setq one-key-menu-call-first-time nil)
                                           (if one-key-popup-window
                                               (one-key-menu-window-open))))))
               (key (one-key-key-description event)))
          (cond (
                 ;; HANDLE KEYSTROKES MATCHING MENU ITEMS
                 ;; (unless the help window is open)
                 (and (not (get-buffer-window (help-buffer)))
                      (catch 'match
                        (loop for item in filtered-list
                              for match-key = (one-key-remap-key-description (caar item))
                              for desc = (cdar item)
                              for rest = (cdr item)
                              for command = (if (commandp rest) rest
                                              (if (one-key-list-longer-than-1-p rest)
                                                  (car rest)
                                                (lambda nil (interactive) (message "Invalid command %S" rest))))
                              do
                              (when (equal key match-key)
                                ;; Update key usage statistics if necessary
                                (if one-key-auto-brighten-used-keys
                                    (progn (one-key-menu-increment-key-usage item)
                                           (if issymbol
                                               (add-to-list 'one-key-altered-menus (symbol-name info-alist)))))
                                ;; We need to close the `one-key' menu window before running the items command.
                                ;; Save previous state of the `one-key' window before doing this.
                                (let* ((old-one-key-popup-window one-key-popup-window)
                                       (one-key-popup-window (one-key-menu-window-exist-p)))
                                  (one-key-menu-window-close)
                                  (setq one-key-menu-call-first-time t) ; allow recursive execution of `one-key-menu'
                                  (call-interactively command) ; call the items command
                                  ;; reopen the `one-key' window if necessary
                                  (if (and one-key-popup-window (or keep-window-p match-recursion-p))
                                      (one-key-menu-window-open))
                                  (setq one-key-popup-window old-one-key-popup-window))
                                (setq one-key-menu-call-first-time nil)
                                ;; throw t if the key matched so that this clause's body is executed, otherwise return nil
                                (throw 'match t))) nil))
                 ;; call the `alternate-function' and if `match-recursion-p' is non-nil wait for next keypress
                 (let ((temp (one-key-handle-last alternate-function self match-recursion-p)))
                   ;; if necessary, propagate the value of `keep-window-p' back up
                   (if match-recursion-p
                       (setq keep-window-p temp))))
                ;; HANDLE SPECIAL KEYS:
                ((assoc* key special-keybindings :test (lambda (x y) (equal x (one-key-remap-key-description y))))
                 ;; call the `alternate-function' and the function associated with the special key
                 ;; if this function returns non-nil then wait for the next keypress
                 (let* ((again (funcall
                                (caddr
                                 (assoc* key special-keybindings
                                         :test (lambda (x y) (equal x (one-key-remap-key-description y)))))))
                        (temp (one-key-handle-last alternate-function self again)))
                   ;; if necessary, propagate the value of `keep-window-p' back up
                   (if again (setq keep-window-p temp))))
                ;; HANDLE ALL OTHER KEYS:
                (t
                 (when execute-when-miss-match-p
                   ;; If `execute-when-miss-match-p' is non-nil then execute the normal command for this key
                   ;; Need to close the `one-key' menu window before running the command.
                   ;; Save the previous state of the `one-key' window before doing this.
                   (let* ((old-one-key-popup-window one-key-popup-window)
                          (one-key-popup-window (one-key-menu-window-exist-p)))
                     (one-key-menu-window-close)
                     (one-key-execute-binding-command key)
                     ;; reopen the `one-key' window if necessary
                     (if (and one-key-popup-window (or keep-window-p miss-match-recursion-p))
                         (one-key-menu-window-open))
                     (setq one-key-popup-window old-one-key-popup-window)))
                 ;; call the `alternate-function' and if `miss-match-recursion-p' is non-nil wait for next keypress
                 (let ((temp (one-key-handle-last alternate-function self miss-match-recursion-p)))
                   ;; if necessary, copy the value of `keep-window-p' from recursive call
                   (if miss-match-recursion-p
                       (setq keep-window-p temp))))))
      ;; all keypresses have now been handled so reset global variables ready for next time
      (setq one-key-menu-call-first-time t)
      (setq one-key-menu-show-key-help nil)
      ;; If `keep-window-p' is non-nil then don't close the `one-key' window,
      ;; just change the focus to the previous window.
      (if keep-window-p (if (equal (buffer-name (window-buffer)) one-key-buffer-name) (other-window -1))
        (one-key-menu-window-close))
      ;; Finally, execute `protect-function' if it's a valid function.
      (if (and protect-function
               (commandp protect-function))
          (call-interactively protect-function)))
    ;; propagate the value of `keep-window-p' back down the stack
    keep-window-p))

(defun one-key-execute-binding-command (key)
  "Execute the command bound to KEY (a string description of a key), unless this command is `keyboard-quit'.
If KEY contains shift, and there is no command bound to that key, then the same key binding with shift removed
will be tried (in accordance with normal emacs behaviour)."
  (let* ((rawkey (elt (eval `(kbd ,key)) 0))
         (mods (event-modifiers rawkey))
         (basic (event-basic-type rawkey))
         (func (key-binding (vector rawkey)))
         (keynoshift (append (remove 'shift mods) (list basic)))
         (func2 (or func
                    (key-binding (vector (event-convert-list keynoshift))))))
    (when (and (not (eq func2 'keyboard-quit))
               (functionp func2))
      (setq last-command-event last-input-event)
      (call-interactively func2))))

(defun one-key-handle-last (alternate-function recursion-function recursion-p)
  "Last function called after handling a key press in the `one-key' menu that's not listed in `one-key-special-keys-alist'.
ALTERNATE-FUNCTION is the alternative function to be executed.
RECURSION-FUNCTION is the recursion function to be executed when option RECURSION-P is non-nil.
The return value of RECURSION-FUNCTION will be returned by this function also."
  ;; Execute alternate function.
  (when (and alternate-function
             (functionp alternate-function))
    (call-interactively alternate-function))
  ;; Recursion execute when argument
  ;; `recursion-p' is `non-nil'.
  (if recursion-p
      (funcall recursion-function)))

(defun one-key-menu-window-exist-p nil
  "Return `non-nil' if `one-key' menu window exists, otherwise return nil."
  (and (get-buffer one-key-buffer-name)
       (window-live-p (get-buffer-window (get-buffer one-key-buffer-name)))))

(defun one-key-menu-window-toggle nil
  "Toggle the `one-key' menu window."
  (if one-key-current-window-state
      (if (and (stringp one-key-current-window-state)
               (get-buffer one-key-current-window-state))
          (one-key-menu-window-close t)
        (setq one-key-current-window-state
              (with-selected-window (previous-window) (buffer-name)))
        (fit-window-to-buffer (get-buffer-window one-key-buffer-name)
                              (frame-height)
                              one-key-menu-window-max-height))
    (one-key-menu-window-open)))

(defun one-key-menu-window-open nil
  "Open the `one-key' menu window."
  ;; Save current window configuration.
  (or one-key-menu-window-configuration
      (setq one-key-menu-window-configuration
            (current-window-configuration)))
  ;; Update key brightnesses if necessary
  (if one-key-auto-brighten-used-keys
      (one-key-menu-brighten-most-used info-alist))
  ;; Generate buffer information.
  (with-current-buffer (get-buffer-create one-key-buffer-name)
    (erase-buffer)
    (goto-char (point-min))
    (save-excursion
      (insert (one-key-highlight-menu (one-key-menu-format filtered-list) names menu-number))))
  ;; Pop `one-key' buffer.
  (pop-to-buffer one-key-buffer-name)
  (set-buffer one-key-buffer-name)
  ;; Adjust height of menu window appropriately.
  ;; If `one-key-current-window-state' is a string then we have switched
  ;; from another menu at full height, and so should make this window full height too.
  (if (stringp one-key-current-window-state)
      (fit-window-to-buffer nil (frame-height) one-key-menu-window-max-height)
    (fit-window-to-buffer nil one-key-menu-window-max-height)
    (setq one-key-current-window-state t))
  ;; set the default menu number
  (setq one-key-default-menu-number menu-number)
  nil)

(defun one-key-menu-window-close (&optional norestore)
  "Close the menu window."
  ;; Kill menu buffer.
  (when (bufferp (get-buffer one-key-buffer-name))
    (if (and (stringp one-key-current-window-state)
             (get-buffer one-key-current-window-state))
        (pop-to-buffer one-key-current-window-state))
    (delete-window (get-buffer-window one-key-buffer-name))
    (kill-buffer one-key-buffer-name)
    (setq one-key-current-window-state nil))
  ;; Restore window layout if `one-key-menu-window-configuration' is valid value.
  (when (and (not norestore)
             one-key-menu-window-configuration
             (boundp 'one-key-menu-window-configuration))
    (set-window-configuration one-key-menu-window-configuration)
    (setq one-key-menu-window-configuration nil)))

(defun one-key-menu-window-scroll-up (&optional down)
  "Scroll up one screen of the `one-key' menu window.
If DOWN is non-nil scroll down instead of up."
  (if (one-key-menu-window-exist-p)
      (ignore-errors
        (with-current-buffer one-key-buffer-name
          (if down (scroll-down) (scroll-up))))))

(defun one-key-menu-window-scroll-up-line (&optional down)
  "Scroll up one line of the `one-key' menu window."
  (if (one-key-menu-window-exist-p)
      (ignore-errors
        (with-current-buffer one-key-buffer-name
          (if down (scroll-up 1) (scroll-down 1))))))

(defun one-key-menu-increment-key-usage (item)
  "Increment the key usage statistic for ITEM."
  (destructuring-bind ((key . desc) . rest) item
    (if (commandp rest)
        (setf (cdr item) (list rest 1))
      (if (and (one-key-list-longer-than-1-p rest)
               (numberp (second rest)))
          (progn (incf (second rest))
                 (setf (cdr item) rest))))))

(defsubst one-key-list-longer-than-1-p (x)
  "Return t if x is a list of length > 1, and is not a command."
  (and (not (commandp x))
       (listp x)
       (listp (cdr x))
       (> (length x) 1)))

(defun one-key-menu-brighten-most-used (info-alist)
  "Set values of menu item colours proportionally according to how often they have been used.
Argument INFO-ALIST is the alist of keys and associated decriptions and functions, or a symbol referencing the list."
  ;; first get min and max keypress values
  (if info-alist
      (let ((menu-alist (if (symbolp info-alist) (eval info-alist) info-alist)))
        (if menu-alist
            (let* ((minmaxvals (loop for ((key . desc) . rest) in menu-alist
                                     for val = (if (one-key-list-longer-than-1-p rest)
                                                   (second rest) 0)
                                     maximize val into max
                                     minimize val into min
                                     finally return (list min max)))
                   (minval (first minmaxvals))
                   (maxval (second minmaxvals))
                   (range (- maxval minval)))
              ;; update the colour value (from HSV) of each item in menu-alist
              (loop for item in menu-alist
                    for ((key . desc) . rest) = item
                    ;; get current keypress value (indicating number of times key has been pressed)
                    for val = (if (one-key-list-longer-than-1-p rest)
                                  (second rest) 0)
                    ;; calculate colour value from keypress value
                    for vval = (if (= range 0)
                                   0.5
                                 (+ (/ (- val minval) (* 2.0 range)) 0.5))
                    ;; get current background and foreground colour
                    for (h s v) = (one-key-get-item-colour item nil 'hsv)
                    for fgcol = (one-key-get-item-colour item t)
                    ;; update value of background colour
                    for newbgcol = (hexrgb-hsv-to-hex h s vval) do
                    (setf (cdar item)
                          (propertize desc 'face (list :background newbgcol
                                                       :foreground fgcol)))))))))

(defun one-key-optimize-col-widths (lengths maxlength)
  "Given a list of the lengths of the menu items, work out the maximum possible number of columns and return their widths.
Actually the function returns a list of cons cells in the form (numrows . width) each of which corresponds to a column in
the optimal assignment and indicates the number of rows and width of that column."
  (let ((nitems (length lengths))
        (ncols 1)
        bestcols)
    (if (< (apply '+ lengths) maxlength)
        (mapcar (lambda (len) (cons 1 len)) lengths)
      (while (progn
               (setq ncols (1+ ncols))
               (let ((itemspercol (make-list ncols (/ nitems ncols)))
                     colspecs)
                 (loop for n to (1- (% nitems ncols)) do (incf (nth n itemspercol) 1))
                 (setq colspecs (loop for colnum to (1- ncols) with sofar = 0
                                      for nrows = (nth colnum itemspercol)
                                      for colwidth = (loop for rownum to (1- nrows)
                                                           for itemnum = (if one-key-column-major-order
                                                                             (+ sofar rownum)
                                                                           (+ colnum (* rownum ncols)))
                                                           maximize (nth itemnum lengths))
                                      summing colwidth into width
                                      collecting (cons nrows colwidth) into specs
                                      do (setq sofar (+ sofar nrows))
                                      finally (return (list width specs))))
                 (if (< (car colspecs) maxlength) (setq bestcols (cadr colspecs)) nil))))
      (or bestcols (list (cons nitems (1- maxlength)))))))

(defun one-key-menu-format (info-alist)
  "Format `one-key' menu window key description text (as displayed by the `one-key-menu' function).
Argument INFO-ALIST is an alist of keys and corresponding descriptions and functions, or a symbol referencing that list.
Each element of this list is in the form: ((key . describe) . command)."
  (let ((items-alist (if (symbolp info-alist) (eval info-alist) info-alist)))
    (if (> (length items-alist) 0)
        (let* ((item-lengths (mapcar (lambda (item) (+ (length (cdar item))
                                                       (length (one-key-remap-key-description (caar item))) 4))
                                     items-alist))
               (colspecs (one-key-optimize-col-widths item-lengths (- (window-width) 3)))
               (numitems (length items-alist))
               (maxcols (length colspecs))
               (maxrow (caar (last colspecs)))
               (extras (% numitems maxcols))
               keystroke-msg)
          (loop for row from 0 to maxrow
                for ncols = (if (= row maxrow) extras maxcols) do
                (loop for col from 0 to (1- ncols) with sofar = 0
                      for (colsize . width) = (nth col colspecs)
                      for itemnum = (if one-key-column-major-order
                                        (+ sofar row)
                                      (+ (* row maxcols) col))
                      for item = (nth itemnum items-alist)
                      for (key . desc) = (car item)
                      for keytext = (format "[%s] %s " (one-key-remap-key-description key) desc)
                      if item do
                      (push keytext keystroke-msg)
                      (push (make-string (- width (length keytext)) ? ) keystroke-msg)
                      (setq sofar (+ sofar colsize)))
                (push "\n" keystroke-msg))
          (mapconcat 'identity (nreverse keystroke-msg) ""))
      "No menu items!")))

(defun one-key-get-menu-item (key menu-alist)
  "Return the member of MENU-ALIST corresponding to key KEY, or nil if no such item exists.
KEY may be a char or the string representation of a char.
MENU-ALIST is a list of `one-key' menu items."
  (let ((thekey (one-key-key-description key)))
    (find-if (lambda (x) (equal (one-key-remap-key-description (caar x)) thekey))
             menu-alist)))

(defun one-key-add-menu-item (key desc contents menu-alist)
  "Add a new item to MENU-ALIST in the form ((KEY . DESC) . CONTENTS), overwriting any item with the same key.
Return the new value of MENU-ALIST after adding the item.
KEY may be a char or the string representation of a char.
DESC must be a string (the description to display in the menu).
CONTENTS may be a command or a list whose first element is a command (it will be executed when KEY is pressed in the menu)."
  (let* ((thekey (one-key-key-description key))
         (item (one-key-get-menu-item thekey menu-alist)))
    (if item (progn (setf (cdar item) desc (cdr item) contents) menu-alist)
      (add-to-list 'menu-alist (cons (cons thekey desc) contents)))))

(defun one-key-open-submenu (name var)
  "Open a menu named NAME with menu alist variable VAR as a submenu of the current menu, replacing it if necessary.
If `one-key-submenus-replace-parents' is non-nil then the current menu will be replaced with the submenu, otherwise
a new menu will be added to the current menu set.
This function will only work if called within the context of the `one-key-menu' function since it depends on the variable
THIS-NAME being dynamically bound."
  (let ((currname this-name))
    (one-key-add-menus name var)
    (if one-key-submenus-replace-parents
        (one-key-delete-menu currname)))
    (setq one-key-menu-call-first-time t)
    (one-key-handle-last nil self t))

(defun one-key-merge-menu-lists (lista listb)
  "Given two one-key menu lists, merge them and return the result.
Any items in LISTB that have the same command as an item in LISTA will be disgarded, and any keys in LISTB that
also occur in LISTA will be exchanged for a new key that doesn't occur in either list."
  (loop for ((key . desc) . cmd) in (nconc lista listb) with usedkeys with usedcmds
        unless (memq cmd usedcmds)
        collect (cons (cons (let ((newkey (if (member key usedkeys)
                                              (one-key-generate-key desc usedkeys)
                                            key)))
                              (push newkey usedkeys)
                              newkey)
                            desc)
                      (progn (push cmd usedcmds) cmd))))

(defun* one-key-create-menus-from-menubar-keymap (keymap &optional (name (number-to-string (random)))
                                                         invalidkeys)
  "Create menu alists for a menu-bar keymap KEYMAP and all sub menus.
Submenus will always be assigned to variables whose names are formed by concatenating NAME with the name of the menu-bar
submenu. If NAME is not supplied then a random number will be used instead.
Keys will be assigned to the items using the `one-key-generate-key' function. To exclude certain keys from being used
set INVALIDKEYS to a list of keys (as key-description strings or chars) to be excluded.
These keys will only be excluded from the toplevel menu, not the submenus."
  (let* (
         ;; if the keymap has only one item which is itself a keymap then use that instead
         (keymap2 (if (= (length keymap) 2)
                      (let* ((item (cadr keymap))
                             (pos (position 'keymap item)))
                        (if pos (nthcdr pos item)
                          (find-if 'keymapp item)
                          keymap))
                    keymap))
         ;; keep a track of which keys have been used
         (usedkeys invalidkeys)
         ;; global variable for the main menu
         (mainvarname (concat "one-key-menu-" name "-alist"))
         (mainvar (intern mainvarname))
         ;; local variable for the one-key menu items
         menu-alist)
    ;; loop over the items in the keymap which have keybindings
    (loop for key being the key-codes of keymap2 using (key-bindings bind)
          for itemiscons = (not (listp (cdr bind)))
          ;; if the item is a keymap (to be used as a submenu), extract it
          for itemkeymap = (if (not itemiscons)
                               (let ((pos (position 'keymap bind)))
                                 (if pos (nthcdr pos bind)
                                   (find-if 'keymapp bind))))
          ;; if the item is a command, extract it
          for itemcmd = (if itemiscons (cdr bind)
                          (and (position 'menu-item bind)
                               (find-if (lambda (x) (and (not (stringp x)) (commandp x))) bind)))
          ;; get a description for the command or submenu
          for desc = (if itemiscons (let ((bind0 (car bind))
                                          (bind1 (cdr bind)))
                                      (if (stringp bind0) bind0
                                        (replace-regexp-in-string "-" " " (symbol-name bind1))))
                       (let* ((bind0 (car bind))
                              ;; use either the first string in the item, or the keyname or the command name
                              (str (or (find-if 'stringp bind)
                                       (and bind0 (symbolp bind0) (symbol-name bind0))
                                       (if itemcmd
                                           (capitalize
                                            (replace-regexp-in-string "-" " " (symbol-name itemcmd)))
                                         "unknown"))))
                         ;; if the item is a submenu highlight it, otherwise don't
                         (cond (itemkeymap
                                (propertize str 'face
                                            (list :background "cyan"
                                                  :foreground one-key-item-foreground-colour)))
                               (t str))))
          ;; get a unique key for the item
          for keystr = (one-key-generate-key desc usedkeys)
          ;; get the command for the one-key menu item
          for cmd = (cond (itemcmd itemcmd) ;if the keymap item is a command then use that
                          (itemkeymap ;if the item is a keymap then create a submenu item for the one-key menu
                           (let* ((desc1 (substring-no-properties desc))
                                  (desc2 (replace-regexp-in-string " " "_" desc1))
                                  (submenuname (concat name "_" desc2))
                                  ;; create an appropriate variable name to hold the submenu
                                  (varname (concat "one-key-menu-" submenuname "-alist"))
                                  ;; create the submenu
                                  (submenuvar (one-key-create-menus-from-menubar-keymap
                                               itemkeymap submenuname))
                                  (submenuitems (eval submenuvar)))
                             (if (> (length submenuitems) 1)
                                 ;; if the submenu contains more than one item create the command to open the submenu
                                 `(lambda nil (interactive)
                                    (one-key-open-submenu ,submenuname ,submenuvar))
                               ;; if the submenu only contains one item, add that item to the current menu,
                               ;; delete the submenu variable, and set cmd to nil so it won't be added later
                               (push (car submenuitems) menu-alist)
                               (unintern varname)
                               nil))))
          for skip = (string-match "^--" desc)
          unless (or skip (not cmd)) do ; skip menu divider and null items
          ;; add the item to the one-key menu list
          (push (cons (cons keystr desc) cmd) menu-alist)
          ;; mark this key as used
          (push keystr usedkeys))
    ;; set the global variable to hold the one-key menu items,
    ;; mark it to be saved on exit, and return it
    (set mainvar menu-alist)
    (add-to-list 'one-key-altered-menus mainvarname)
    mainvar))

(defun* one-key-create-menus-from-keymap (keymap &optional
                                                 (name (if (symbolp keymap)
                                                           (replace-regexp-in-string
                                                            "-map$" "" (symbol-name keymap))
                                                         "unknown"))
                                                 prefix)
  "Create menu alists for a keymap and all sub keymaps.
KEYMAP is the keymap or keymap symbol to use, NAME is a name for the keymap (e.g. \"emacs-lisp-mode\") and will
be used to remove common prefix words from the item descriptions.
If a symbol is supplied for keymap then by default NAME will be set to the symbols name but with \"-map\" removed from
the end (if present).
Variables will be created for storing the menus, and the variable for the main menu will be returned.
The main variable will be named one-key-menu-NAME-alist, and the submenus variables will be named
 one-key-menu-NAME-???-alist (where ??? is the name of the submenu).
If the PREFIX arg is present then key descriptions in the menu will be prefixed by this arg (this is used when the
function is called recursively).

Any submenus that have fewer than `one-key-min-keymap-submenu-size' items will be merged with their parent menu,
unless this would create a menu of more than (length one-key-default-menu-keys) items.
Also any items whose commands have keybindings that are in `one-key-disallowed-keymap-menu-keys' will have new keys
created for them."
  (let* ((name (or name  ; make sure the name variable is set properly
                   (if (symbolp keymap)
                       (replace-regexp-in-string
                        "-map$" "" (symbol-name keymap))
                     "unknown")))
         (keymap1 (if (functionp keymap) (symbol-function keymap)
                    (if (symbolp keymap) (eval keymap)
                      keymap)))         ;get the keymap value
         (menubar (lookup-key keymap1 [menu-bar])) ;get any menu-bar items
         (mainvar (intern (concat "one-key-menu-" name "-alist"))) ;variable to hold the main menu
         (winwidth (window-width)) ;need to know the window width to make sure descriptions aren't too long
         usedkeys menu-alist)
    ;; loop over the keys in the keymap
    (loop for key being the key-codes of keymap1 using (key-bindings cmd) with usedcmds
          ;; get the key description
          for keystr = (if (consp key) ; could be a cons cell representing a range of keys if we have a char-table
                           (one-key-key-description (car key))
                         (one-key-key-description key))
          for keydesc = (if prefix
                            (if (equal prefix "ESC")
                                (concat "M-" keystr)
                              (concat prefix " " keystr))
                          keystr)
          ;; skip null keys, menu-bar items (these will be added later), duplicate commands,
          ;; and shadowed keys (possible if keymap inherits from another keymap) 
          unless (or (string-match one-key-null-keys keystr)
                     (eq key 'menu-bar)
                     (memq cmd usedcmds)
                     (member keystr usedkeys))
          ;; If the item is a command...
          do (cond ((commandp cmd)
                    (let* (
                           ;; create the item description from the command name or sexp,
                           ;; and add the key description to the end
                           (keymapname (replace-regexp-in-string "mode$\\|mode-map$\\|map$" "" name))
                           (keymapnameregex (regexp-opt (list keymapname (capitalize keymapname))))
                           (desc1 (if (symbolp cmd) (symbol-name cmd) (format "%S" cmd)))
                           (desc1a (replace-regexp-in-string keymapnameregex "" desc1))
                           (desc1b (capitalize (replace-regexp-in-string "-" " " desc1a)))
                           (desc1c (concat desc1b " (" keydesc ")"))
                           (startchar (max 0 (- (+ (length keydesc) 6 (length desc1c))
                                                (if one-key-force-multi-column-keymap-menus
                                                    (/ winwidth 2)
                                                  winwidth))))
                           (desc2 (substring desc1c startchar))
                           ;; if the key is invalid, generate a new one
                           (keystr2 (if (member keystr one-key-disallowed-keymap-menu-keys)
                                        (one-key-generate-key desc2 usedkeys)
                                      keystr)))
                      ;; mark this command as used
                      (push cmd usedcmds)
                      ;; mark the key as used and add the item to the menu list
                      (push keystr2 usedkeys)
                      (push (cons (cons keystr2 desc2) cmd) menu-alist)))
                   ;; If the item is a keymap..
                   ((keymapp cmd)
                    (let* (
                           ;; create an appropriate name and description for the submenu
                           (submenuname (concat name "_" keystr))
                           (desc2 (concat "Prefix key (" keydesc ")"))
                           (desc3 (propertize desc2 'face
                                              (list :background "cyan"
                                                    :foreground one-key-item-foreground-colour)))
                           ;; if the key is invalid, generate a new one
                           (keystr2 (if (member keystr one-key-disallowed-keymap-menu-keys)
                                        (one-key-generate-key desc2 usedkeys)
                                      keystr))
                           (existingvar (intern-soft (concat "one-key-menu-" submenuname "-alist"))))
                      ;; if a submenu for this prefix key has already been created then merge this one with it
                      (if (and existingvar (member keystr usedkeys))
                          (let ((menuvar (one-key-create-menus-from-keymap
                                          cmd (concat submenuname "temp") keydesc)))
                            (set existingvar (one-key-merge-menu-lists (eval existingvar) (eval menuvar)))
                            ;; delete the temporary variable
                            (unintern (symbol-name menuvar)))
                        ;; otherwise call this function recursively to create a new submenu
                        (let* ((menuvar (one-key-create-menus-from-keymap cmd submenuname keydesc))
                               ;; number of items in the submenu
                               (numnewitems (length (eval menuvar))))
                          ;; if the number of items in the submenu is small then merge it with the parent menu
                          (if (and (< numnewitems one-key-min-keymap-submenu-size)
                                   (< (+ (length menu-alist) numnewitems) (length one-key-default-menu-keys)))
                              (progn (setq menu-alist (one-key-merge-menu-lists menu-alist (eval menuvar)))
                                     (unintern (symbol-name menuvar)))
                            ;; otherwise create a link to it in the parent menu
                            (let ((cmd2 `(lambda nil (interactive) (one-key-open-submenu ,submenuname ,menuvar))))
                              (push keystr usedkeys)
                              (push keystr2 usedkeys) ; mark key as used
                              (push (cons (cons keystr2 desc3) cmd2) menu-alist)))))))))
    ;; if there are menu-bar items, add them if user agrees
    (if (and menubar
             (or (eq one-key-include-menubar-items t)
                 (and (eq one-key-include-menubar-items 'prompt)
                      (y-or-n-p "Include menu-bar items?"))))
        ;; we use no name for the menubar menu since the symbol will subsequently be uninterned anyway
        (let ((menuvar (one-key-create-menus-from-menubar-keymap menubar (concat name "-menubar") usedkeys)))
          (setq menu-alist (one-key-merge-menu-lists menu-alist (eval menuvar)))
          (unintern (symbol-name menuvar))))
    ;; set the value of the variable to hold the main menu, and make sure it will be saved if necessary
    (set mainvar menu-alist)
    (add-to-list 'one-key-altered-menus (symbol-name mainvar))
    ;; return the main menu variable
    mainvar))

 (defun one-key-generate-key (desc &optional usedkeys elements trykey)
   "Return a key for the menu item whose description string is DESC.
 The generated key can be used in a `one-key' menu.
 If provided, ELEMENTS should be a list of keys to choose from, (otherwise `one-key-default-menu-keys' will be used),
 USEDKEYS should be a list of keys which cannot be used (since they have already be used),
 and TRYKEY is a key which will be returned if it is not in USEDKEYS (otherwise another key will be found).
 This function can be used to help automatic creation of `one-key' menus."
   (let ((elements (or elements one-key-default-menu-keys)))
     (or (and trykey
              (not (memq trykey usedkeys))
              (not (member (one-key-key-description trykey) usedkeys))
              trykey)
         (loop for element in elements
               for keystr = (one-key-key-description element)
               if (not (or (memq element usedkeys)
                           (member keystr usedkeys)))
               return (one-key-key-description element))
         (error "Can not generate a unique key for menu item : %s" desc))))

(defvar one-key-key-description-remap
  '(("<return>" . "RET")
    ("<tab>" . "TAB")
    ("<space>" . "SPC")
    ("<begin>" . "<home>")
    ("<escape>" . "ESC")
    ("<C-escape>" . "C-ESC")
    ("<delete>" . "DEL"))
  "Alist of key descriptions and their preferred versions.
This is required in order that keys such as RET (which can also be described as <return> are always described and
recognized the same way.")

(defun one-key-remap-key-description (keydesc)
  (let ((pair (assoc keydesc one-key-key-description-remap)))
    (if pair (cdr pair) keydesc)))

(defun one-key-key-description (keyseq)
  "Return the key description for the key sequence or single key KEYSEQ.
KEYSEQ may be a vector, integer, symbol or string representing a key sequence, or nil.
If KEYSEQ is nil then nil is returned, if it is non-nil and not a string, vector, symbol or number then an error is flagged."
  (one-key-remap-key-description
   (cond ((not keyseq) nil)
         ((vectorp keyseq) (key-description keyseq))
         ((numberp keyseq) (single-key-description keyseq))
         ((symbolp keyseq) (single-key-description keyseq))
         ((stringp keyseq) (if (string-match "^C-\\|^M-\\|^RET\\|^SPC\\|^TAB\\|^<[a-z0-9-]+>\\|^[a-zA-Z0-9]" keyseq)
                               keyseq
                             (key-description keyseq)))
         (t (error "Invalid key sequence: %S" keyseq)))))

(defun one-key-append-keys-to-descriptions (descriptions keys)
  "Append key descriptions for keys in KEYS to corresponding descriptions in DESCRIPTIONS, and return result.
DESCRIPTIONS should be a list of menu item descriptions and KEYS should be a list of keys as numbers, vectors or strings.
KEYS should be the same length as DESCRIPTIONS.
If any elements of key are nil then the corresponding description will be left alone.
If any element of descriptions is nil it will be left as nil."
  (flet ((addkeys (desc key)
                  (let ((keystr (one-key-key-description key)))
                    (if desc
                        (if keystr (concat desc " (" keystr ")")
                          desc)
                      nil))))
    (mapcar* 'addkeys descriptions keys)))

(defun one-key-append-numbers-to-menu-name (menuname nummenus)
  "Return list of menu names formed by appending numbers to MENUNAME.
The new names will be in the form \"MENUNAME (N)\" where N runs over the integers from 1 to NUMMENUS.
This is useful for creating menu types that return multiple menus."
    (loop for num from 1 to nummenus
        collect (concat menuname " (" (number-to-string num) ")")))

(defun* one-key-create-menu-lists (commands &optional descriptions keys
                                            (maxsize (length one-key-default-menu-keys))
                                            (keyfunc 'one-key-generate-key))
  "Create list/lists of menu items for use in `one-key' menu.
COMMANDS should be a list of commands for the menu items, and KEYS an optional corresponding list of keys.
If any element in KEYS is nil, or if KEYS is nil, then KEYFUNC will be used to generate a key for the item.
DESCRIPTIONS is an optional argument which should contain a list of descriptions for the menu items.
If any of the items in DESCRIPTIONS is nil or if DESCRIPTIONS is not supplied then the item will have its description
set from the corresponding command name.
If the number of menu items is larger than MAXSIZE then several menus will be created, each of
which contains at most MAXSIZE items. By default MAXSIZE is equal to the length of `one-key-default-menu-keys',
and KEYFUNC is set to `one-key-generate-key' (which selects keys from `one-key-default-menu-keys')."
  (let* ((nitems (length commands))
         (nitemslast (% nitems maxsize))
         (nummenus (+ (/ nitems maxsize) (min nitemslast 1)))
         (indices (loop with start = 0
                        with end = 0
                        while (< end nitems)
                        do (setq start end end (min (+ end maxsize) nitems))
                        collect (cons start end)))
         (menu-alists (loop for (start . end) in indices
                            for cmds = (subseq commands start end)
                            for descs = (subseq descriptions start end)
                            for keys2 = (subseq keys start end)
                            for descs2 = (loop for desc in descs
                                               for cmd in cmds
                                               for key in keys2
                                               for desc2 = (or desc
                                                               (capitalize
                                                                (replace-regexp-in-string
                                                                 "-" " " (symbol-name cmd))))
                                               collect (if key
                                                           (concat desc2 " ("
                                                                   (one-key-key-description key)
                                                                   ")")
                                                         desc2))
                            for usedkeys = (loop for key in keys2 if key collect key)
                            for keystrs = (loop for key in keys2
                                                for desc in descs2
                                                collect (or (one-key-key-description key)
                                                            (let ((newkey (one-key-generate-key desc usedkeys)))
                                                              (push newkey usedkeys)
                                                              newkey)))
                            collect (loop for cmd in cmds
                                          for desc in descs2
                                          for key in keystrs
                                          collect (cons (cons key desc) cmd)))))
    menu-alists))
                                                          
(defun one-key-build-menu-sets-menu-alist nil
  "Build menu-alist for opening menu sets defined in `one-key-sets-of-menus-alist'."
  (let* ((descriptions (mapcar (lambda (item)
                                 (let ((str (car item)))
                                   (if (equal str one-key-default-menu-set)
                                       (propertize str 'face (list :background "red"
                                                                   :foreground one-key-item-foreground-colour))
                                     str))) one-key-sets-of-menus-alist))
         (commands (mapcar (lambda (item)
                 `(lambda nil (interactive)
                    (one-key-open-menu-set ,(car item))))
                           one-key-sets-of-menus-alist)))
    (car (one-key-create-menu-lists commands descriptions))))

(defun one-key-save-altered-menus nil
  "Save the menus listed in `one-key-altered-menus' into the file `one-key-menus-save-file'.
Any menu names that match the regular expressions in `one-key-exclude-from-save' will not be saved."
  (loop for x in one-key-altered-menus
        for varname = (if (stringp x) x (if (symbolp x) (symbol-name x)))
        for name = (if (string-match "one-key-menu-.*-alist" varname)
                       (substring varname 13 -6))
        for var = (intern-soft varname)
        for menulist = (eval var)
        for exclude = (loop for regex in one-key-exclude-from-save
                            if (string-match regex varname) return t)
        if (and name var (not exclude)) do (one-key-save-menu name var menulist)))

(defun one-key-get-major-mode-menu (name)
  "Return a menu name and menu alist for the current major mode.
This function is used by `one-key-types-of-menu' and the NAME argument is redundant.
A cons cell in the form (menu-name . menu-alist) will be returned.
If there is an element of `one-key-major-mode-remap-alist' associated with the current major mode then that will be used,
otherwise the name of the current major mode will be used.
In both cases if the variable `one-key-menu-<menu-name>-alist' (where <menu-name> is the menu name associated with this
major mode) exists then it will be used, otherwise it will be created."
  (let* ((menuname (or (cdr (assoc major-mode one-key-major-mode-remap-alist))
                       (with-selected-window
                           (previous-window)
                         (symbol-name major-mode))))
         (symname (concat "one-key-menu-" menuname "-alist"))
         (menusym (intern-soft symname)))
    (if (or (not menusym) (not (boundp menusym)))
        (let* ((mapname (concat menuname "-map"))
               (mapsym (intern-soft mapname)))
          (if (and mapsym (boundp mapsym))
              (progn (one-key-create-menus-from-keymap mapsym menuname)
                     (setq menusym (intern-soft symname)))
            (message "Can't create menu for %S" major-mode)
            (setq menusym nil))))
    (cons menuname menusym)))

(defun one-key-create-blank-menu (name)
  "Prompt user for a name, create a blank one-key menu and return a cons cell containing the name and the menu."
  (let* ((name (read-string "Menu name: "))
         (symname (concat "one-key-menu-" name "-alist"))
         (menusym (intern-soft symname)))
    (while menusym
      (if (not (y-or-n-p "Menu with that name already exists, overwrite?"))
          (progn (setq name (read-string "Menu name: "))
                 (setq symname (concat "one-key-menu-" name "-alist"))
                 (setq menusym (intern-soft symname)))
        (setq menusym nil)))
    (setq menusym (intern symname))
    (set menusym nil)
    (add-to-list 'one-key-altered-menus (symbol-name menusym))
    (cons name menusym)))

(defun one-key-retrieve-existing-menu (x)
  "Prompt for an existing one-key menu and return it in a cons cell along with its name."
  (let* ((names (loop for sym being the symbols
                      for name = (symbol-name sym)
                      when (string-match "one-key-menu-\\(.+\\)-alist" name)
                      collect (match-string 1 name)))
         (name (if (featurep 'ido)
                   (ido-completing-read "Menu: " names)
                 (completing-read "Menu: " names))))
    (cons name (intern-soft (concat "one-key-menu-" name "-alist")))))

(defun one-key-create-menu-from-existing-keymap (name)
  "Prompt the user for a keymap and return a one-key menu for it along with it's name, in a cons cell."
  (let* ((names (loop for sym being the symbols
                      when (or (keymapp sym)
                               (and (boundp sym)
                                    (keymapp (symbol-value sym))))
                      collect (symbol-name sym)))
         (name (if (featurep 'ido)
                   (ido-completing-read "Keymap: " names)
                 (completing-read "Keymap: " names)))
         (partname (replace-regexp-in-string "-map$" "" name))
         (kmap (intern-soft name)))
    (one-key-create-menus-from-keymap kmap partname)
    (cons name (intern-soft (concat "one-key-menu-" partname "-alist")))))

(defun one-key-create-menu-from-prefix-key-keymap (keystr)
  "Prompt the user for a prefix key and return a one-key menu for it along with it's name, in a cons cell."
  (interactive (let ((keystr (read-string "Enter the emacs string representation of the required prefix keys: ")))
                 (while (not (ignore-errors (read-kbd-macro keystr)))
                   (setq keystr (read-string "Invalid key sequence! Try again: ")))
                 (list keystr)))
  (let (keysequence kmap)
    (setq keysequence (read-kbd-macro keystr))
    (setq kmap (key-binding keysequence))
    (if (keymapp kmap)
        (let* ((kmap2 (if (functionp kmap) (symbol-function kmap)
                        (if (symbolp kmap) (eval kmap) kmap)))
               (desc1 (replace-regexp-in-string " " "_" keystr))
               (desc2 (replace-regexp-in-string "#" "\\\\#" desc1))
               (desc3 (concat "Prefix-Key:" desc2)))
          (cons desc3 (one-key-create-menus-from-keymap kmap2 desc3 desc2)))
      (error "No keymap is currently associated with that prefix key!"))))

(defun one-key-prefix-key-menu-command (keystr &optional submenup)
  "Given prefix key description KEYSTR open a one-key menu containing the commands associated with that prefix key.
If SUBMENUP is non-nil then the `one-key-open-submenu' command is used to add/replace a menu in the current menu set."
  (interactive (let ((keystr (read-string "Enter the emacs string representation of the required prefix keys: ")))
                 (while (not (ignore-errors (read-kbd-macro keystr)))
                   (setq keystr (read-string "Invalid key sequence! Try again: ")))
                 (list keystr)))
  (let* ((pair (one-key-create-menu-from-prefix-key-keymap keystr))
         (name (car pair))
         (menu (cdr pair)))
    (if submenup
        (one-key-open-submenu name menu)
    (one-key-menu name menu))))

(defun one-key-create-menu-sets-title-format-string nil
  "Return a title format string for menu sets one-key menus."
  (let* ((col1 (if one-key-auto-brighten-used-keys "#7FFF00000000" "red"))
         (col2 (cdr (assq 'background-color (frame-parameters))))
         (hsv2 (hexrgb-hex-to-hsv col2))
         (col2a (hexrgb-hsv-to-hex (first hsv2) (second hsv2) 0.5))
         (col2b (if one-key-auto-brighten-used-keys col2a col2))
         (keystr1 (propertize "default menu set"
                              'face (list :background col1
                                          :foreground one-key-item-foreground-colour)))
         (keystr2 (propertize "normal menu set"
                              'face (list :background col2b
                                          :foreground one-key-item-foreground-colour))))
    (concat keystr1 keystr2 "\n"
            (format "Sorted by %s (%s first). Press <f1> for help.\n"
                    one-key-current-sort-method
                    (if one-key-column-major-order "columns" "rows")))))

(defun one-key-submit-bug-report nil
  "Submit a bug report for one-key via mail."
  (interactive)
  (require 'reporter)
  (let ((reporter-prompt-for-summary-p "Bug report subject: "))
    (reporter-submit-bug-report
     one-key-maintainer-email
     (concat "one-key version " one-key-version)
     (list 'one-key-altered-menus
           'one-key-auto-brighten-used-keys
           'one-key-autosave-menus
           'one-key-buffer-name
           'one-key-column-major-order
           'one-key-copied-items
           'one-key-current-item-being-moved
           'one-key-current-sort-method
           'one-key-current-window-state
           'one-key-default-menu-keys
           'one-key-default-menu-number
           'one-key-default-menu-set
           'one-key-default-sort-method-alist
           'one-key-default-special-keybindings
           'one-key-default-title-func
           'one-key-disallowed-keymap-menu-keys
           'one-key-exclude-from-save
           'one-key-include-menubar-items
           'one-key-item-foreground-colour
           'one-key-major-mode-remap-alist
           'one-key-menu-call-first-time
           'one-key-menu-sets-special-keybindings
           'one-key-menu-show-key-help
           'one-key-menu-window-configuration
           'one-key-menu-window-max-height
           'one-key-menus-save-file
           'one-key-min-keymap-submenu-size
           'one-key-mode-line-format
           'one-key-mode-line-message
           'one-key-null-keys
           'one-key-persistent-menu-number
           'one-key-popup-window
           'one-key-sets-of-menus-alist
           'one-key-special-keybindings
           'one-key-submenus-replace-parents
           'one-key-menu-toplevel-alist
           'one-key-types-of-menu)
     nil nil
     "Remember to cover the basics, that is, what you expected to happen and
what in fact did happen.
Please mention which major mode was active and what keys were pressed at the time of the fault.

To read how to make a good bug report see:

http://www.gnu.org/software/emacs/manual/html_node/emacs/Understanding-Bug-Reporting.html
------------------------------------------------------------------------")))

    
;; Set one-key menu types
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "top-level"
                            (lambda (name) (equal name "top-level"))
                            (cons "top-level" 'one-key-menu-toplevel-alist)
                            one-key-default-title-func nil) t)
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "blank menu"
                            (lambda (name) (equal name "blank menu"))
                            'one-key-create-blank-menu
                            one-key-default-title-func nil) t)
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "major-mode"
                            (lambda (name) (string-match "-mode$" name))
                            'one-key-get-major-mode-menu
                            one-key-default-title-func nil) t)
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "existing menu"
                            (lambda (name) (equal name "existing menu"))
                            'one-key-retrieve-existing-menu
                            one-key-default-title-func nil) t)
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "existing keymap"
                            (lambda (name) (equal name "existing keymap"))
                            'one-key-create-menu-from-existing-keymap
                            one-key-default-title-func nil) t)
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "Prefix-Key"
                            (lambda (name) (string-match "Prefix-Key" name))
                            (lambda (name)
                              (let* ((keystr1 (if (> (length name) 11) (substring name 11) nil))
                                     (keystr2 (if keystr1 (replace-regexp-in-string "_" "" keystr1))))
                                (if keystr2
                                    (one-key-create-menu-from-prefix-key-keymap keystr2)
                                  (call-interactively 'one-key-create-menu-from-prefix-key-keymap))))
                            one-key-default-title-func nil) t)
(one-key-add-to-alist 'one-key-types-of-menu
                      (list "menu-sets"
                            (lambda (name) (equal name "menu-sets"))
                            (lambda (name)
                              (cons name (one-key-build-menu-sets-menu-alist)))
                            'one-key-create-menu-sets-title-format-string
                            'one-key-menu-sets-special-keybindings) t)

;; add function for autosaving menus to kill-emacs-hook
(add-hook 'kill-emacs-hook (lambda nil (if one-key-autosave-menus (one-key-save-altered-menus))))

;; Load the saved one-key menus.
(if one-key-menus-save-file
    (if (file-readable-p one-key-menus-save-file)
        (load-file one-key-menus-save-file)
      (message "Can't read file %s" one-key-menus-save-file))
  (message "`one-key-menus-save-file' is not set, no menus loaded"))


(provide 'one-key)

;;; one-key.el ends here


