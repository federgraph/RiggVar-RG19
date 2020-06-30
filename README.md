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

I am about to create another branch and immediately continue in master.

In the branch I will continue the attempt to reconcile old and new, but not now.
Instead I want press ahead and put the new VCL app in the master branch, mainly by deleting old code.

Note that I started the repo by uploading old code, very close to the original version of 1997.

I did so while I already had newer versions of the app  - with less features - but with better graphics and for other compilers, including fpc.
The fpc version will be in another repo.

## Branchpoint RG68

This branch may end up being - or continue to be - the feature richest app, for desktop with main menu and status bar.

In addition to the main menu it still has some old forms:
- TInputForm which is not strictly needed any more, but it would be bad to throw it away.
- TOutputForm which is not strictly needed any more.

TRggModule, the old controller which has been replace by TRggMain, is needed to support TInputForm and TOutputForm.
Unfortunately some work is needed because it not exactly compatible with the new regime of doing things. 

## I jumped !

I have just taken an aggressive step by including a VCL version of the button frame, which is great for touch screen.

> But original code is still present - in branch RG19ABC.

Now, suddenly, I have very new code and very old code in the same project.

- I am in the middle of transition.
- You can watch me as I go.
- Trying to reconcile old with new.
- Want to rescue some features, which did not make it into the published version.
- This repo will continue to use GDI for drawing, and have no dependencies.
- Old code not yet tested on high-dpi screen.
- The master branch is work in progress.
