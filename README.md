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

- I started this repo by uploading old code, very close to the original version of 1997.
- I did so while I already had newer versions of the app  - with fewer features but better graphics (the FMX version).
- The goal is to keep this repo close in functionality to both the FMX and LCL version.
- This is the VCL version, it (still) contains GDI drawing code.

Note that there are two permanent branches.

## Branch RG68

This branch may end up being - or continue to be - the feature richest version - for the desktop, with main menu and status bar.

## Branch RG19ABC

Use the legacy layout to learn how the application looked in the past.

- RG19A is an MDI application (oldest)
- RG19B is an SDI application (old)
- RG19C is better than RG19B ?

## About sibling projects at GitHub

Similar projects are available for FMX and LCL:
- The LCL project RG 51 can be built in the Lazarus IDE on iMac.

| Project | Platform | Note |
| :-- | :- | :- |
| RG19 | VCL | this repo, original RiggVar project |
| [RG38](https://github.com/federgraph/RiggVar-RG38) | FMX | Trimm 420 |
| [RG51](https://github.com/federgraph/RiggVar-RG51) | LCL | FPC / Lazarus project using TBGRABitmap |
