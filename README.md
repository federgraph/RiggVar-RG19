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

I started this repo by uploading old code, very close to the original version of 1997.

I did so while I already had newer versions of the app  - with less features - but with better graphics and using other compilers.
- FMX version is in repository RiggVar-RG38
- LCL version is currently in a private repository.

The goal is to keep this repo very close in functionality to both the FMX and LCL versions.

> This is the VCL version.

It still contains GDI drawing code.

Note that there are two permanent branches.

## Branch RG68

This branch may end up being - or continue to be - the feature richest app, for the desktop, with main menu and status bar.

## Branch RG19ABC

- RG19A is an MDI application (oldest)
- RG19B is an SDI application (old)
- RG19C is better than RG19B, in my opinion.

## About sibling projects at GitHub

Similar projects are available for FMX and LCL:
- The LCL project can be built in the Lazarus IDE on iMac.
- RG51 is currently in a private repository on GitHub.

| Project | Platform | Note |
| :-- | :- | :- |
| RG19 | VCL | original RiggVar project |
| [RG38](https://github.com/federgraph/RiggVar-RG38) | FMX | Trimm 420 |
| RG51 | LCL | FPC / Lazarus project using TBGRABitmap |
