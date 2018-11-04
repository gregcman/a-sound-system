# a-sound-system


- can load whatever audio file format ffmpeg supports, which is a lot. Can play mp3s, oggs, wavs, etc..
- Audio player is openal but a different backend could probably be used. 

IMPORTANT NOTE:

The AVFormatContext c structure might need some tweaking depending on the version, since it uses (in the C source code) c macros to detect the operating system, so the structure might be different on different operating systems. There is a #+darwin in one of the files in this project for mac osx, it worked last time, but is now commented out and somehow works on linux. I am too lazy to figure out what happened.
