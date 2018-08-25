// insert any functions that are useful throughout the experiment here
var shuffleComb = function(comb) {
    // while this one is trivial, this just to show that we CAN define a function here
    return _.shuffle(comb);
};

function coinFlip(head, tail) {
  return (Math.floor(Math.random() * 2) == 0) ? head : tail;
};

function anchor(high, low) {
  if (coinFlip(high, low) == high) {
    question = question_high
  } else {
    question = question_low
  }
}

function success() {
  if(document.getElementById("answer").value==="") {
        document.getElementById('next').disabled = true;
    } else {
        document.getElementById('next').disabled = false;
    }
}
