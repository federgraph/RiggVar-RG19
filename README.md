# RiggVar-RG19

This is the original (legacy) Delphi VCL Rigg Application.

Other Names:
- RiggVar
- Rgg19
- RG19

Most of the code was developed between October 1995 and January 1998,
based on paper and pencil work done in 1991 and 1992.

Original developer: Gustav Schubert.

This project is now public under the GPL license.

> This repository is in maintenance mode.

> Use this project for reference only.

> Delphi development will now continue in RiggVar-RG38 (FMX).

I have been working recently on RG19 with the goal of getting old and new sources (VCL and FMX) in sync as much as possible (including bug fixes).
This will help me to rescue some goodies that have been lost in between, during my attempt to build a (tablet) version for the store.

Note that this is why - here in this project - we have two competing User Interfaces, which is certainly confusing when you look at the code.
In terms of Model View Controller, there are two controllers, which try to drive old and new views, respectively.

How everything is supposed to work will be more clear in the new repo.
Note that the FMX app (over there) will have better graphics.
Here we still have GDI graphics which is based on Integer coordinates with no antialiasing.
If you want to see textual rReports in a TMemo, the VCL application (here) is still of good use.

## Building

I suggest latest Delphi CE (Community Edition).

## Using the Application

Start by using the wheel of the mouse, Shift-Wheel and Ctrl-Wheel.

## Helping

You could provide concrete data for your boat (420), and test out if you can set up everything so that the predictions of the app match what you can measure at the real boat, on shore, before you take to the lake or sea.

I need help to adapt the program for other, similarly rigged boats.

And much more.

## Notes

- Most of the existing documentation pieces, e.g drawings, relate most directly to this project.
- Where and how to publish documentation still to be decided.
- Over the years I have ported parts of the code to java, c#, and 
typescript.
- The old Delphi project contains features that have not yet been surfaced in any of the newer apps.
- The new Delphi project is currently the most advanced.
- The new data format should be usable on all platforms.
- No dependency on any third party components in this project.
- Hit F9 in the Delphi IDE and go.

