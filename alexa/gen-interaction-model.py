#!/usr/bin/python
# ZCode -> Alexa grammar generator
# Requires ztools 'infodump'

import sys, os, argparse, json, subprocess, re

def read_customisations(filename):
    excludes = []
    replacements = {}
    if filename:
        with open(filename, 'rt') as fp:
            for line in fp:
                if line[0] == '-':
                    excludes.append(line[1:].strip())
                elif "=" in line:
                    a,b = line.split("=")
                    replacements[a.strip()] = b.strip()
    def apply(w):
        if w in excludes: return None
        return replacements.get(w, w)
    return apply

def invoke_infodump(*args):
    cmd = " ".join(args)
    infodump = subprocess.Popen(args, stdout=subprocess.PIPE)
    return infodump.stdout.readlines()

def generate_noun_slot(verbs, index):
    if "go" in verbs:
        root = "Direction"
        type = "LIST_OF_DIRECTIONS"
    else:
        root = "Object"
        type = "LIST_OF_NOUNS"
    return {
        "name": "%s%c" % (root, ord('A') + index),
        "type": type
    }

def generate_verb_slot(index):
    return {
        "name": "Verb%d" % (index,),
        "type": "LIST_OF_VERBS"
    }

def parse_sentence(verbs, sentence, preprocessor):
    num_verb_slots = 0
    num_noun_slots = 0
    words = []
    for word in map(preprocessor, sentence.split()):
        if word in verbs:
            word = "{%s}" % (generate_verb_slot(num_verb_slots)["name"],)
            num_verb_slots += 1
        if word == 'OBJ':
            word = '{%s}' % (generate_noun_slot(verbs, num_noun_slots)["name"],)
            num_noun_slots += 1
        words.append(word)
    return words

def parse_grammar(grammar, preprocessor):
    all_verbs = set()
    all_utterances = set()
    finding_verb = True
    for line in grammar:
        if finding_verb:
            if "verb = " in line:
                verb_words = re.findall(r'\"(\w+)\"', line)
                verbs = filter(None, map(preprocessor, verb_words))
                if len(verbs) > 0:
                    finding_verb = False
        else:
            for sentence in re.findall(r'"([\w\s]+)"', line):
                utterances.add(parse_sentence(verbs, sentence, preprocessor))

            if line.strip() == "":
                all_verbs.update(verbs)
                all_utterances.update(utterances)
                finding_verb = True

    return sorted(all_verbs), sorted(all_utterances)

def parse_words(zobjects, word_type, preprocessor):
    words = set()
    noun = re.compile(r'\[\s*\d+\]\s+@\s+\$[\da-f]+\s+(\w+).*\<' + word_type + '\>')
    for line in zobjects:
        m = noun.match(line)
        if m:
            words.add(m.group(1))
    return filter(None, map(preprocessor, words))

def count_verb_slots(utterance):
    return len(filter(lambda w: w[:5] == "{Verb", utterance))

def count_noun_slots(utterance):
    return len(filter(lambda w: w[:7] == "{Object", utterance))

def generate_schema(verbs, utterances):
    verb_slots = max(map(count_verb_slots, utterances))
    noun_slots = max(map(count_noun_slots, utterances))
    intent = {'intent': 'GameMoveIntent'}
    if verb_slots > 0:
        intent['slots'] = []
        for i in range(slot_count):
            intent['slots'].append(generate_slot(verb, i))
    return intent


def dump_strings(strings, file):
    with open(file, 'wt') as fp:
        fp.writelines(s + "\n" for s in strings)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="zcode to alexa tool")
    parser.add_argument("--zfile", help="The zcode file to parse", required=True)
    parser.add_argument("--customise", help="File containing customisation rules")
    parser.add_argument("--schema", help="The schema file to generate", default="intent-schema.txt")
    parser.add_argument("--nouns", help="The nouns slot file to generate", default="nouns.txt")
    parser.add_argument("--directions", help="The directions slot file to generate", default="directions.txt")
    parser.add_argument("--utterances", help="The utterances file to generate", default="utterances.txt")
    parser.add_argument("--infodump", help="Path to infodump tool", default="./infodump")
    args = parser.parse_args(sys.argv[1:])

    preprocessor = read_customisations(args.customise)

    zobjects = invoke_infodump(args.infodump, "-w0", "-c1", "-d", args.zfile)
    nouns = parse_words(zobjects, 'noun', preprocessor)
    directions = parse_words(zobjects, 'dir', preprocessor)

    grammar = invoke_infodump(args.infodump, "-w0", "-s", "-g", args.zfile)
    verbs, utterances = parse_grammar(grammar, preprocessor)
    schema = generate_schema(verbs, utterances)

    with open(args.schema, 'wt') as fp:
        json.dump(schema, fp, indent=2)

    dump_strings(utterances, args.utterances)
    dump_strings(nouns, args.nouns)
    dump_strings(directions, args.directions)
