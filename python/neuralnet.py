import sys

if not hasattr(sys, 'argv'):
    sys.argv = ['']

# import dynet_config
# dynet_config.set_gpu()
import dynet as dy
import random
from collections import defaultdict
from itertools import count
from ocaml import io_pairs, input_vocab, output_vocab

SEQUENCE_LENGTH = 300
LAYERS = 2
INPUT_DIM = SEQUENCE_LENGTH
HIDDEN_DIM = 300

input_vocab = list(input_vocab)
input_vocab.append("<EOS>")
int2input_token = list(input_vocab)
input_token2int = {t:i for i,t in enumerate(input_vocab)}
VOCAB_SIZE = len(input_vocab)

output_vocab = list(output_vocab)
output_vocab.append("<???>")
output_vocab.append("<EOS>")
int2output_token = list(output_vocab)
output_token2int = {t:i for i,t in enumerate(output_vocab)}
OUTPUT_VOCAB_SIZE = len(output_vocab)

training_pairs = []
for f in io_pairs:
    eos = ("<EOS>", "<EOS>")
    input_size = SEQUENCE_LENGTH - 2
    if len(f) <= input_size:
        training_pairs += [eos] + f + [eos]
    else:
        training_pairs += [[eos] + f[i:i+input_size] + [eos]
                           for i in range(0, len(f)-input_size)]
print(len(training_pairs), " training pairs")

pc = dy.ParameterCollection()


srnn = dy.SimpleRNNBuilder(LAYERS, INPUT_DIM, HIDDEN_DIM, pc)
lstm = dy.LSTMBuilder(LAYERS, INPUT_DIM, HIDDEN_DIM, pc)

# add parameters for the hidden->output part for both lstm and srnn
params_lstm = {}
params_srnn = {}
for params in [params_lstm, params_srnn]:
    params["lookup"] = pc.add_lookup_parameters((VOCAB_SIZE, INPUT_DIM))
    params["R"] = pc.add_parameters((OUTPUT_VOCAB_SIZE, HIDDEN_DIM))
    params["bias"] = pc.add_parameters((OUTPUT_VOCAB_SIZE))

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
    return " ".join(out)

def train(rnn, params, sequence):
    trainer = dy.SimpleSGDTrainer(pc)
    iterations = 50
    for i in range(iterations):
        loss = do_one_sequence(rnn, params, sequence)
        loss_value = loss.value()
        loss.backward()
        trainer.update()
        if i % 10 == 0:
            print("iteration %d of %d" % ((i+1), iterations))
            input_sequence = [i for (i, o) in sequence]
            output_sequence = [o for (i, o) in sequence]
            print("%.10f" % loss_value)
            print(" ".join(output_sequence))
            print(generate(rnn, params, input_sequence))

def run():
    for i, sequence in enumerate(training_pairs):
        print("Training sequence %d of %d. %.5f%% complete" %
              (i+1, len(training_pairs), float(i)/len(training_pairs)))
        train(srnn, params_srnn, sequence)
    pc.save("nn.model")
