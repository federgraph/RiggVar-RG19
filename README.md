# RiggVar-RG19

VCL version of Trimm 420.

```
(*
-
-     F
-    * * *
-   *   *   G
-  *     * *   *
- E - - - H - - - I
-  *     * *         *
-   *   *   *           *
-    * *     *             *
-     D-------A---------------B
-              *
-              (C) federgraph.de
-
*)
```

## Branch RG68

In this branch I will continue the attempt to reconcile old and new.

Old features to retain:
- TInputForm which is not strictly needed any more, but it would be bad to throw it away.
- TOutputForm.

TRggModule, the old controller which has been replaced by TRggMain, is needed to support TInputForm and TOutputForm.
Unfortunately some work is needed because it is not exactly compatible with the new regime of doing things. 

Branch RG68 may end up being - or continue to be - the feature richest app, for the desktop, with a main menu and a status bar.

## Some History

I started the repo by uploading old code, very close to the original version of 1997.
And I did so while I already had newer versions of the app - with less features.

Branch RG19ABC was created just before taking an aggressive step forward (the Jump) by including a VCL version of the button frame,
which is supposed to be great for touch.
In order to do so I needed to delete incompatible code.
Some know problems appeared, which I will NOT sort out immediately.
Instead I created a permanent branch point RG68, for the work to continue. 

Then - in the master branch - I was now free to delete more code, and making remaining files identical to existing project RG67.

Now, if you are located in the master branch and look back: Branch RG68 adds features to RG67!

Both branches, RG68 and RG19ABC are considered permanent branches.

- RG19 in master branch: matches RG67
- RG19 in branch RG68: desktop app with main menu and status bar
- RG19A in branch RG68ABC: old MDI application
- RG19B in branch RG68ABC: old SDI application
- RG19C in branch RG68ABC: latest code behind combined with old forms.

The Rigg model code (in folder Core) is kept up to date, across the branches.
It is the UI that is still old.

Future derived projects will use other graphics technology, and may use other compilers.