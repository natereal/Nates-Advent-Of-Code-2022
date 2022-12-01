# Nate's Advent of Code 2022

These are my solutions to year 2022 of [Advent Of Code](https://adventofcode.com). I will be using R for my solutions, and using tjmahr's excellent
package [aoc](https://github.com/tjmahr/aoc), which uses an R package project structure for organising solutions to advent of code.

I like to use R is because it is the language I'm most familiar with. 

Using an R package structure may seem a bit overkill, but by using it we get a lot of really nice features for free such as automatically generated templates, unit tests, and an easy way for others to run your solutions (they just install your package!)
By using the function aoc::use_day(), puzzle inputs will automatically be downloaded (if you set up your cookie correctly), generate .R files from templates, and functions for testing which are useful for making sure your code doesn't break when modifying previous 
solutions; I will generally try to optimise solutions after I've solved them because my first attempts are often pretty messy.

aoc will assume your package is named adventofcodeXX where XX are the last two digits of the year, but you can customise this if you like (I didn't bother but it's probably a good idea if you are planning to install other people's solutions as packages).

To get your cookie for automatic downloads in Safari, open one of your puzzle inputs (you can use a previous year if you like), right on the page and select "Inspect Element", and you can find it under the "Storage" tab:

![image](https://user-images.githubusercontent.com/33176127/205165702-dea2f12e-2e4e-4bd4-b564-23698ece694b.png)

Add this cookie to a file named .aoccookie in the root of your project directory in the following format:
```
session=PASTECOOKIEHERE
```

You should probably exclude this from your git repo.
