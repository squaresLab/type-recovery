from typing import Dict, List, Tuple
import sys

if not hasattr(sys, 'argv'):
    sys.argv = ['']


import os.path
# Random seeding
import random
seed = random.randrange(4294967295)
rng = random.Random(seed)

import dynet_config
dynet_config.set(mem=2048, random_seed=seed)
# dynet_config.set_gpu()
import dynet as dy

import datetime
from collections import defaultdict
from itertools import count
from ocaml import io_pairs, input_vocab, output_vocab
from pathlib import Path

# Colors for printing
RED = "\x1b[1;31m"
GREEN = "\x1b[1;32m"
YELLOW = "\x1b[1;33m"
BLUE = "\x1b[1;34m"
END_COLOR = "\x1b[0m"

# Location to save the model and data
MODEL_FILE = "nn.model"
STATS_FILE = "stats.txt"

# Typed versions of the OCaml imports
typed_io_pairs: List[List[Tuple[str, str]]] = io_pairs
typed_input_vocab: List[str] = list(input_vocab)
typed_output_vocab: List[str] = list(output_vocab)

int2input_token = list(typed_input_vocab)
input_token2int = {t:i for i,t in enumerate(typed_input_vocab)}
VOCAB_SIZE = len(typed_input_vocab)

typed_output_vocab.append("<???>")
int2output_token = list(typed_output_vocab)
output_token2int = {t:i for i,t in enumerate(typed_output_vocab)}
OUTPUT_VOCAB_SIZE = len(typed_output_vocab)

SEQUENCE_LENGTH = 300
LAYERS = 2
INPUT_DIM = SEQUENCE_LENGTH
HIDDEN_DIM = SEQUENCE_LENGTH / 2

training_pairs: List[List[Tuple[str, str]]] = []
for f in typed_io_pairs:
    input_size = SEQUENCE_LENGTH
    if len(f) <= input_size:
        training_pairs += [f]
    else:
        training_pairs += [f[i:i+input_size] for i in range(0, len(f)-input_size)]
rng.shuffle(training_pairs)
TRAINING_PAIRS = len(training_pairs)
print(TRAINING_PAIRS, " training pairs")
print(len(typed_output_vocab)-1, " possible types")

# set up the neural net
pc = dy.ParameterCollection()
srnn = dy.SimpleRNNBuilder(LAYERS, INPUT_DIM, HIDDEN_DIM, pc)
params: Dict[str, dy.Expression] = {}
params["lookup"] = pc.add_lookup_parameters((VOCAB_SIZE, INPUT_DIM))
params["R"] = pc.add_parameters((OUTPUT_VOCAB_SIZE, HIDDEN_DIM))
params["bias"] = pc.add_parameters((OUTPUT_VOCAB_SIZE))

# Load training data from disk if it exists
model = Path(MODEL_FILE)
if model.is_file():
    print("Model file found, loading... ", end="")
    try:
        pc.populate(MODEL_FILE)
        print("OK")
    except Exception as e:
        print("Failed")
        print("Message was:\n\t%s" % (str(e), ))
else:
    print("No model file found")

# return compute loss of RNN for one sequence
def do_one_sequence(rnn, params, sequence):
    # setup the sequence
    dy.renew_cg()
    s0 = rnn.initial_state()

    R = params["R"]
    bias = params["bias"]
    lookup = params["lookup"]
    input_sequence = [input_token2int[t] for (t, _) in sequence]
    output_sequence = [output_token2int[t] for (_, t) in sequence]
    s = s0
    loss = []
    for input_token, output_token in zip(input_sequence, output_sequence):
        s = s.add_input(lookup[input_token])
        probs = dy.softmax(R*s.output() + bias)
        loss.append( -dy.log(dy.pick(probs,output_token)) )
    loss = dy.esum(loss)
    return loss

# generate from model:
def generate(rnn, params, input_sequence):
    def sample(probs):
        rnd = random.random()
        for i,p in enumerate(probs):
            rnd -= p
            if rnd <= 0: break
        return i

    # setup the sentence
    dy.renew_cg()
    s = rnn.initial_state()

    R = params["R"]
    bias = params["bias"]
    lookup = params["lookup"]

    out=[]
    for i in input_sequence:
        s = s.add_input(lookup[input_token2int[i]])
        probs = dy.softmax(R*s.output() + bias)
        probs = probs.vec_value()
        out.append(int2output_token[sample(probs)])
    return out

def train(rnn, params, sequence, progress, completed):
    trainer = dy.SimpleSGDTrainer(pc)
    iterations = 100
    for i in range(iterations):
        loss = do_one_sequence(rnn, params, sequence)
        loss_value = loss.value()
        loss.backward()
        trainer.update()
        if i % 10 == 0:
            print("\n")
            print("Training sequence %d of %d. " % (progress, TRAINING_PAIRS), end="")
            print("Iteration %d of %d " % ((i+1), iterations), end="")
            print("%.2f%% complete" % (completed, ))
            input_sequence = [i for (i, o) in sequence]
            output_sequence = [o for (i, o) in sequence]
            guessed_sequence = generate(rnn, params, input_sequence)
            report = []
            print("Loss: %.10f" % loss_value)
            for token in input_sequence:
                if token == "<???>":
                    print(YELLOW, token, END_COLOR, end=" ")
                else:
                    print(token, end=" ")
            print()
            for actual, guessed in zip(output_sequence, guessed_sequence):
                if actual == guessed:
                    if actual == "<--->":
                        report += [BLUE + guessed + END_COLOR]
                    else:
                        report += [GREEN + guessed + END_COLOR]
                else:
                    report += [RED + guessed + END_COLOR]
            for token in report:
                print(token, end=" ")
            print()

def run():
    start_time = datetime.datetime.now()
    progress = 0
    try:
        for sequence in training_pairs:
            completed = (float(progress) / TRAINING_PAIRS) * 100
            progress += 1
            train(srnn, params, sequence, progress, completed)
    finally:
        finish_time = datetime.datetime.now()
        with open(STATS_FILE, "w") as stats:
            stats.write("Started at %s\n" % (start_time.ctime(), ))
            stats.write("Finished at %s\n" % (finish_time.ctime(), ))
            stats.write("%d/%d completed (%.2f%%)\n" %
                        (progress, TRAINING_PAIRS, completed))
            stats.write("Seed: %d\n" % (seed, ))
        pc.save(MODEL_FILE)
