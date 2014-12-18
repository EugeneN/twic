# README #

Twic is a simple reader of a Twitter user's home feed. It's main goal is a search of a better Twitter feed usage experience. Currently it has next features:

- reasonably minamalistic visual appearance, with just a list of tweets and a Refresh button;

- every tweet item displays also a minimal set of data - only author's avatar and a tweet text;

- tweet text is rendered with minimal visual distractions, using only a basic typography;

- tweet's embedded content if any is shown directly, without a need to click on it to expand;

- older tweets are on the top, newer on the bottom, providing a most natural way for reading;

- read tweets are not shown. Once a user reads to the bottom of the feed he/she sees a Refresh button with a count of new, yet unread tweets. After clicking on the button the old, already seen tweets are replaced with new ones.

- The Refresh button is grey if there is no new tweets in a feed, or orange if there are new itwems.

All the checks are done automatically in background. User has a very simple workflow - if the single button in user interface is orange, then he/she may click on it and get new tweets. The button becomes grey until next tweets will be available.

Compare with the original Twitter UI:

<img src="res/compare.png" style="width: 600px;"/>




