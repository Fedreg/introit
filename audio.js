
const ctx = new AudioContext || new webkitAudioContext

function playNote(note) {
  let hz       = note.hz;
  let octave   = note.octave;
  let duration = note.duration;
  let tempo    = note.tempo || 1;
  let wave     = note.wave || "square";
  let start    = note.startTime || ctx.currentTime;

  let osc  = ctx.createOscillator();
  let vol  = ctx.createGain();
  let gain = vol.gain;
  let fhz  = hz * (Math.pow(2, (octave - 1)));

  osc.type = wave;
  osc.gain = 0;
  osc.frequency.value = fhz;

  gain.setTargetAtTime(0.50, start, 0.05);
  gain.setTargetAtTime(0.00, start + ((duration / tempo) - (0.05 / tempo)), 0.05);

  osc.connect(vol);
  vol.connect(ctx.destination);

  osc.start(start);
  osc.stop(start + (duration / tempo) + 0.5);
}

function lastPlayedNoteDuration(playedNotes) {
  if (playedNotes.length === 0) {
    return 0;
  }
  else {
    let len = playedNotes.length;
    let lastNote = playedNotes[len - 1];
    return lastNote.duration || lastNote[0].duration;
  }
}

function timeoutLength(tempo) {
  let offset = tempo <= 100 ? 1 : tempo * 0.01;
  return 50.0 / offset;
}

var newState = {};

function scheduler(state) {
  let queued = state.queuedNotes;
  let played = state.playedNotes;
  let nnt    = state.nextNoteTime || ctx.currentTime;
  let t      = state.tempo;
  let firstQ = queued[0];
  let restQ  = queued.slice(1);

  if (nnt < (ctx.currentTime + 0.05) && queued.length > 0) {
      let tempoMod = t / 60;
      let lastDur  = lastPlayedNoteDuration(played);
      let newNnt   = nnt + (lastDur / tempoMod);
      played.push(firstQ);
      newState     = {"queuedNotes": restQ,
	              "playedNotes": played,
	              "tempo": t,
	              "nextNoteTime": newNnt}
    if (Array.isArray(firstQ) == true) {
      firstQ.map(n => {
	n.startTime = newNnt;
	n.tempo     = tempoMod;
	playNote(n)
      });
    }
    else {
      firstQ.startTime = newNnt;
      firstQ.tempo     = tempoMod;
      playNote(firstQ);
    }
  }
  if (queued.length > 0) {
    setTimeout(function() {scheduler(newState)}, timeoutLength(t));
  }
}

// TEST; run these in the console
// playNote({"hz":260, "startTime": 0, "octave":1, "duration":1, "tempo":1});
// lastPlayedNoteDuration([{"duration": 1}]);
// scheduler({"queuedNotes":[{"hz":260, "octave":1, "duration":1, "tempo":1},{"hz":200, "octave":1, "duration":1, "tempo":1}], "playedNotes": [], "nextNoteTime": 0, "tempo": 100});

