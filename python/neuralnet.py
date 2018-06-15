import sys

if not hasattr(sys, 'argv'):
    sys.argv = ['']

import dynet as dy
import random
from collections import defaultdict
from itertools import count
from ocaml import io_pairs, vocab

LAYERS = 2
INPUT_DIM = 300
HIDDEN_DIM = 300

vocab = list(vocab)
vocab.append("<EOS>")
int2tok = list(vocab)
tok2int = {t:i for i,t in enumerate(vocab)}
VOCAB_SIZE = len(vocab)

training_pairs = []
for f in io_pairs:
    eos = ("<EOS>", "<EOS>")
    if len(f) <= 298:
        training_pairs += [eos] + f + [eos]
    else:
        training_pairs += [[eos] + f[i:i+298] + [eos] for i in range(0, len(f)-298)]

pc = dy.ParameterCollection()


srnn = dy.SimpleRNNBuilder(LAYERS, INPUT_DIM, HIDDEN_DIM, pc)
lstm = dy.LSTMBuilder(LAYERS, INPUT_DIM, HIDDEN_DIM, pc)

# add parameters for the hidden->output part for both lstm and srnn
params_lstm = {}
params_srnn = {}
for params in [params_lstm, params_srnn]:
    params["lookup"] = pc.add_lookup_parameters((VOCAB_SIZE, INPUT_DIM))
    params["R"] = pc.add_parameters((VOCAB_SIZE, HIDDEN_DIM))
    params["bias"] = pc.add_parameters((VOCAB_SIZE))

# return compute loss of RNN for one sequence
def do_one_sequence(rnn, params, sequence):
    # setup the sequence
    dy.renew_cg()
    s0 = rnn.initial_state()

    R = params["R"]
    bias = params["bias"]
    lookup = params["lookup"]
    input_sequence = [tok2int[t] for (t, _) in sequence]
    output_sequence = [tok2int[t] for (_, t) in sequence]
    s = s0
    loss = []
    for input_token, output_token in zip(input_sequence, output_sequence):
        s = s.add_input(lookup[input_token])
        probs = dy.softmax(R*s.output() + bias)
        loss.append( -dy.log(dy.pick(probs,output_token)) )
    loss = dy.esum(loss)
    return loss

# generate from model:
def generate(rnn, params):
    def sample(probs):
        rnd = random.random()
        for i,p in enumerate(probs):
            rnd -= p
            if rnd <= 0: break
        return i

    # setup the sentence
    dy.renew_cg()
    s0 = rnn.initial_state()

    R = params["R"]
    bias = params["bias"]
    lookup = params["lookup"]

    s = s0.add_input(lookup[tok2int["<EOS>"]])
    out=[]
    while True:
        probs = dy.softmax(R*s.output() + bias)
        probs = probs.vec_value()
        next_token = sample(probs)
        out.append(int2tok[next_token])
        if out[-1] == "<EOS>": break
        s = s.add_input(lookup[next_token])
    return " ".join(out[:50]) # strip the <EOS>

def train(rnn, params, sequence):
    trainer = dy.SimpleSGDTrainer(pc)
    for i in range(200):
        loss = do_one_sequence(rnn, params, sequence)
        loss_value = loss.value()
        loss.backward()
        trainer.update()
        if i % 5 == 0:
            print("%.10f" % loss_value, end="\t")
            print(generate(rnn, params))

def run():
    for sequence in training_pairs:
        train(srnn, params_srnn, sequence)
