#### Trying ELM

I love trying elm as I am backend developer and was trying to find options for learnign UI.

To be a full stack developer

### Tools or Commands

>// To build and create ls file
> elm make src/Main.elm --optimize --output=elm.js
>//To start server
>elm reactor --port=8001

### flow
we first created a file with .elm extension and put all of our Elm code in it. We then compiled it to JavaScript using elm make. After that we referenced the output file (elm.js) from index.html to load our Elm application into the browser.

That was a lot of steps just to see the result of our code. Luckily, Elm provides an interactive development tool called elm reactor that lets us see the result of our code instantly. Go to the root directory in terminal and run this command: elm reactor. You should see something like this: