import * as Tone from 'tone'

var app = Elm.Main.init({ node: document.getElementById("elm") });


const synth = new Tone.Synth().toDestination();

//synth.triggerAttackRelease("C4", "8n");

app.ports.playNote.subscribe(function(message) {
synth.triggerAttackRelease("C4", "8n");
      console.log("Pete2", message)
});
