---
title: Autoplay permissions
author:
- "[@MonoidMusician](https://blog.veritates.love/)"
# date: 2024/10/23
---

This is a page allowing you to opt-in to autoplay permissions for this website.

You should see some sort of permissions dialogue or indicator, maybe to the left of the address bar.

If that does not work, as of early 2025 these instructions seem to work:

- Chromium on Desktop: click on the info icon next to the address bar, click on site settings at the bottom, change the “Sound” permission to “Allow”

  Alternatively: [Settings](chrome://settings) > [Privacy & Security](chrome://settings/privacy) > [Site Settings](chrome://settings/content) > Content > Additional Content Settings > [Sound](chrome://settings/content/sound) > Allowed to play sound > Add https://blog.veritates.love:443
- Firefox on Desktop: [Settings > Privacy & Security](about:preferences#privacy) > Permissions > Autoplay > Settings > https://blog.veritates.love:443 > Allow Audio and Video

  (You cannot *add* a site there, which is the purpose of this page to force the site to show up there.)
- Safari on Desktop: Settings > Website > Auto-Play > blog.veritates.love > Allow All Auto-Play

  <!-- Lack of Web MIDI: https://news.ycombinator.com/item?id=23676109 -->

Then you can go <a id="back">back</a>.

Here is a blank autoplay video:

<!-- blank-intro-videos/blank.mp4 -->
<video style="background: currentColor" width="20px" height="20px" autoplay src="data:video/mp4;base64,AAAAGGZ0eXBpc29tAAAAAGlzb21tcDQxAAAACGZyZWUAAAAmbWRhdCELUCh9wBQ+4cAhC1AAfcAAPuHAIQtQAH3AAD7hwAAAAlNtb292AAAAbG12aGQAAAAAxzFHd8cxR3cAAV+QAAAYfQABAAABAAAAAAAAAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADAAAAG2lvZHMAAAAAEA0AT////xX/DgQAAAACAAABxHRyYWsAAABcdGtoZAAAAAfHMUd3xzFHdwAAAAIAAAAAAAAYfQAAAAAAAAAAAAAAAAEAAAAAAQAAAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAEAAAAAAAAAAAAAAAAAAAWBtZGlhAAAAIG1kaGQAAAAAxzFHd8cxR3cAAKxEAAAL/xXHAAAAAAA0aGRscgAAAAAAAAAAc291bgAAAAAAAAAAAAAAAFNvdW5kIE1lZGlhIEhhbmRsZXIAAAABBG1pbmYAAAAQc21oZAAAAAAAAAAAAAAAJGRpbmYAAAAcZHJlZgAAAAAAAAABAAAADHVybCAAAAABAAAAyHN0YmwAAABkc3RzZAAAAAAAAAABAAAAVG1wNGEAAAAAAAAAAQAAAAAAAAAAAAIAEAAAAACsRAAAAAAAMGVzZHMAAAAAA4CAgB8AQBAEgICAFEAVAAYAAAANdQAADXUFgICAAhIQBgECAAAAGHN0dHMAAAAAAAAAAQAAAAMAAAQAAAAAHHN0c2MAAAAAAAAAAQAAAAEAAAADAAAAAQAAABRzdHN6AAAAAAAAAAoAAAADAAAAFHN0Y28AAAAAAAAAAQAAACg="></video>

<script>
(function() {
  var back = document.getElementById("back");
  back.onclick = () => history.back();
  back.href = 'javascript:void(0)';
})();
</script>
