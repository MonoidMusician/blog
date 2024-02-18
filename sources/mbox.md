---
title: Viewing `.mbox` emails, offline!
subtitle: Out of Gmail and into Thunderbird
author:
- "[@MonoidMusician](https://cofree.coffee/~verity/)"
date: 2024/02/07
---

Some email services like Gmailʼs Google Takeout will export your files in a `.mbox` file format.^[`.mbox` stands for “mailbox”, and it is actually a [very ugly format](https://en.wikipedia.org/wiki/Mbox#Mbox_family) under the hood, which is probably why nobody really supports or advertises it.]

Iʼve looked several times to find decent ways to view these archives, since me and my friends lose access to our G Suite emails after we graduate school. (Sigh …)

[Thunderbird](https://www.thunderbird.net/) is the best software to view `.mbox` files, especially since it is free and open source software!
[Several](https://documentation.its.umich.edu/mbox-thunderbird) [universities](https://computing.sas.upenn.edu/importingtakeoutthunderbird) [recommend](https://itsupport.umd.edu/itsupport?id=kb_article_view&sysparm_article=KB0011941) [it](https://td.ucmo.edu/TDClient/34/Portal/KB/ArticleDet?ID=353).
Unfortunately some of those instructions are out of date.
And itʼs a little tricky to get right for my friendʼs exact use case.

:::{.Key_Idea box-name=Goals}
Our goals are to

- Archive emails from Gmail.
- View them later without any account.
- Store the data on an external drive, instead of taking up limited space on the computer.
:::

These instructions should work for any OS that Thunderbird runs on.
If you donʼt know what that means, you should be good to go :)

:::Note
You can use Thunderbird as a mail client normally.
Thatʼs definitely the easier way to do it.

Like, it will fetch your emails from the account for you.
(Does it download the full backlog? I think so?)

This is just a guide to use Thunderbird *unconventionally*: to use it offline to view `.mbox` files.
:::

I wrote a lot of words in this guide, just to be thorough and explicit!^[Also to hopefully future-proof it, so when Thunderbird changes in a couple years these instructions could still be followed.]

It is not that difficult to carry out, I promise <3

It should take maaaybe 20 minutes to set up Thunderbird and load your emails?

:::{.Key_Idea box-name="tl;dr"}
You can skip to [The Actual Import!!](#the-actual-import) if you roughly know what are doing.

- Right click on “Local Folders” > ImportExportTools NG > Import mbox Files > Individual mbox Files
:::

<!-- Skip to the highlighted boxes if you roughly know what you are doing. -->

## Download your archive

If you want to export from Gmail, go to the Google Takeout website: https://takeout.google.com/.

0. Make sure you are signed in to the right email account (click on your profile picture in the top right).
1. Select what information you want in your archive:

    - You might want to hit “Deselect all” first.
    - You need to select Mail to get the archive `.mbox`.
      Hereʼs what it says:

      > **Mail**
      >
      > Messages and attachments in your Gmail account in MBOX format. User settings from your Gmail account in JSON format.
    - You probably also want contacts, since they are stored separately.

      > **Contacts**
      >
      > Contacts and contact photos you added yourself, as well as contacts saved from your interactions in Google products like Gmail.
    - If you store a lot of stuff in Google Drive, that will be the main item that determines how much data you will have to download, so maybe you want to save it for another archive.
      Just be mindful of that!
2. Technical settings:

    - You probably want “Send download link via email”: this is the most direct and easiest method, and does not use cloud storage.
      But if you want to store it in the cloud, pick one of the other options.
    - Frequency: Export once.
    - File type & size: I recommend choosing `.zip` and 50GB, or follow your preferences / the advice that Google gives. It doesnʼt really matter.
3. Wait for your export to finish.
  This will take longer depending on how much data your are exporting.^[My 30GB archive of Gmail+Drive took maybe 6 hours until it was available? I donʼt remember exactly. My much smaller 30MB archive of only emails took just a few minutes.]
4. Ideally, download your archive directly to your *external* drive.

    If you have enough space to download it to your internal drive and move it later, thatʼs fine.

    But if you donʼt and it downloads it automatically without asking where to put it, you might want to cancel it and go into your browserʼs settings:

    - Safari: open Preferences, General tab, set “File download location” to “Ask for each download”
    - Firefox: open Settings, search for “Downloads”, enable “Always ask you where to save files”.
    - Chrome/etc.: open Settings, search for “Downloads”, enable “Ask where to save each file before downloading”.

5. Extract your archive.
  It will be named something like `takeout-20240101T123456Z-001.zip`.

  You only need the `Mail` directory, if you can choose what to extract.
  Actually you only need `Mail/All mail Including Spam and Trash.mbox`.

## Set up Thunderbird

Download and install Thunderbird: https://www.thunderbird.net/.

### Create a fake account in Thunderbird

:::Note
If you want to set up and use a real email, that is easier and lets you skip this!

But it **is not** and **should not** be necessary to have a real email to view archives.
Hmph.
:::

We will be using a feature called “Local Folders”, which is a local account that Thunderbird maintains, not tied to any external servers.

Unfortunately we need to set up a fake account first to get access to Local Folders.^[It was not always this way, as far as I can tell …]

When it says “Set up your existing email address”, weʼll do our best to decline its offer:

- Enter something for “Your full name”, it could just be “a”.
- Enter something for “Email address”, it could just be “w\@w”.
- Now you should see an option to “Configure manually”. Click that!
- Next you should see “Incoming server” and “Outgoing server”.
  Ignore these and click “Advanced config” below them.
- “Confirm Advanced Configuration”: OK.

Now you should be taken to **Account Settings** where you can immediately delete the “account” you made.

- Click on “Account Actions” (near the bottom left) and “Remove Account”: confirm that you want to remove “w\@w”.

### Set up external storage for Local Folders

Restart Thunderbird now.
This is necessary for the next step.

When Thunderbird is restarted, go back to **Account Settings** and click on the “Local Folders” account that is now unlocked.

It will read:

> Account Settings
>
> The following is a special account. There are no identities associated with it.

This is the account we will be using to import the `.mbox` file.

:::{.Key_Idea box-name="Key Step"}
The Local Folders option we are looking to change is called “Local Directory”.

Click browse on it, navigate to your external drive, and create a new folder.
You might want to call it something like “Thunderbird Email Storage” so you remember what it is for when you see it on your external drive.
:::

Click “Select” when youʼve done that.
(You either need to be in the new folder, or have it selected from its parent folder.)

Thunderbird will ask you to restart (again).
Do so.

### Import the archive into Thunderbird

Weʼll be using an add-on called “ImportExportTools NG” to make this a little easier.

:::{.Bonus box-name="Aside"}
The funny note is that this tool is not necessary.
If you are comfortable with files, you can install it yourself by moving the `.mbox` file to [the right location](https://www.reddit.com/r/Thunderbird/comments/17k76cx/importexporttools_ng_wont_import_mbox_over_4gb/).
But itʼs nice to not worry about getting that right.
:::

Install the tool:

- Find the **Add-ons Manager** (in the menu, or from **Account Settings**).
- Search for “ImportExportTools NG” and install it.
  (Search for “ImportExport” if you donʼt want to type everything.)
- It will ask you to confirm, and warn you:

  > This extension will have permission to:
  >
  > Have full, unrestricted access to Thunderbird, and your computer.

  This is okay – it is not a malicious add-on.
  We wonʼt be using it for much and you are free to delete it after the import.

:::{.Key_Idea box-name="The Actual Import!!" #the-actual-import}
After installing the helper, we can finally do the import:

- Make sure you are in a mail view, not Account Settings.
- Right click on **“Local Folders”**.
  This is the important part, that I kept missing!
- Click through these menu items:
  - ImportExportTools NG
    - Import mbox Files
      - Individual mbox Files
- Find your downloaded `.mbox` archive and click Open!
:::

This will take a while, and Thunderbird should give you a status indicator at the bottom of the window.

> Processing All mail Including Spam and Trash.mbox : 123.45 MB

Let it do its thing until it is done.

Then you can rename the created mailbox in Thunderbird.
Right-click on it and change its name from “All mail Including Spam and Trash” to, oh, I donʼt know,

> ~~Cringe~~ Fond College Memories

## Tada!

That should be all you need!

You should be able to open Thunderbird and read your emails, any time that your external drive is plugged in.

You should be able to search through emails to find what you need.

In fact, thereʼs a lot more things you can do with Thunderbird!
(I actually donʼt know that much about Thunderbird.)

If you like it, you might consider using it for your other emails too :3

## Addenda

Extra steps and details:

### Cleanup

After you are done importing, you can delete the downloaded version(s) that were used to import.

You may want to keep some of these extra copies around, as backups and redundant backups!
Iʼm just telling you where to delete them if you want to or need the space.

- You might have a copy of the archive on your internal drive, if you downloaded there.
  (Looks like `takeout-20240101T123456Z-001.zip`.)
  This occurs if you drag and drop from your internal drive to an external drive: it copies the file by default, and youʼll have to delete the internal copy manually.
- If you extracted all of the files you want out of your archive, you can delete the archive on your external drive too.
- You can delete the `All mail Including Spam and Trash.mbox` now that it has all been imported into Thunderbird.

You can also remove the add-on we used (ImportExportTools NG).

### Usage

You should be able to open Thunderbird and read your emails, any time that your external drive is plugged in.

:::Warning
You will have to close Thunderbird to eject the external drive safely.

Safely ejecting the drive is important if a program is writing data to it.
:::

On the other hand, if you open Thunderbird and the external drive is not plugged in, it should not cause any issues.
You just wonʼt see any of those emails until you close Thunderbird, plug in the drive, and reopen it.

### (Optional) Set Thunderbird to Offline

This is not important, but you may want to be extra careful I guess.

How to set Thunderbird to Offline mode:

- Find “Settings” or “Thunderbird Settings”
<!-- - Stay on the General tab
- Scroll down to the Network & Disk Space section
- Offline subsection, Offline button -->
- Search for “Offline”, click the “Offline” button.
- Uncheck “Automatically follow detected online state”
- “Manual state when starting up”: set to Offline.
- Set the other settings according to your preferences.

### Delete for good

If you want to delete the mailbox you imported into Thunderbird, you have two options:

- You can delete the mailbox in Thunderbird.

  Deleting a large mailbox made my Thunderbird unresponsive for a while, but you can just let it do its thing and it should finish soon enough.

  I used to always spam the “Wait” button, out of fear that it would be interrupted, but this is not necessary, it turns out!^[I am very neurotypical and never anxious, I assure you.] ^[Actually jk, I guess neurotypicals are anxious sometimes. Therefore I assure you I am a normal amount of anxious!]
- You could instead find where you put Thunderbirdʼs data on your external drive and delete it from there too.
  I suggested you name it “Thunderbird Email Storage”.

### Need more help?

The university links cover different parts of the process that I skimmed over:

- https://documentation.its.umich.edu/mbox-thunderbird
- https://computing.sas.upenn.edu/importingtakeoutthunderbird
- https://itsupport.umd.edu/itsupport?id=kb_article_view&sysparm_article=KB0011941
- https://td.ucmo.edu/TDClient/34/Portal/KB/ArticleDet?ID=353

Just be warned that they are out of date in several ways.

### P.S.

Feel free to ping me if this goes out of date, I am happy to maintain it.

Arguably this is the wrong place for the information to live, but eh … Iʼm trying.
