#!/usr/bin/env node
const Elm = require('../dist/index.js')

const app = Elm.Main.worker();

app.ports.tick.subscribe((t) => {
  var time = new Date(t);
  var timeString = time.toLocaleTimeString();
  var leftPad = " ".repeat(Math.round((process.stdout.columns / 2) - (timeString.length / 2)));
  process.stdout.write(leftPad + timeString + "\r");
});
