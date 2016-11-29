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

def slot_name(verb, index):
    root = "Direction" if verb == "go" else "Object"
    return "%s%c" % (root, ord('A') + index)

def parse_sentence(verb, sentence, preprocessor):
    num_objects = 0
    words = []
    for word in map(preprocessor, sentence.split()):
        if word == 'OBJ':
            word = '{%s}' % (slot_name(verb, num_objects),)
            num_objects += 1
        words.append(word)
    return " ".join(words), num_objects

def generate_utterances(intent_name, sentence, main_verb, synonyms):
    yield intent_name + " " + sentence
    for synonym in synonyms:
        yield intent_name + " " + sentence.replace(main_verb, synonym)

def generate_intent(verb, intent_name, slot_count):
    intent = {'intent': intent_name}
    if slot_count > 0:
        intent['slots'] = []
        for i in range(slot_count):
            intent['slots'].append({
                'name': slot_name(verb, i),
                'type': 'LIST_OF_NOUNS'
            })
    return intent

def parse_grammar(grammar, preprocessor):
    intents = []
    all_utterances = set()
    finding_verb = True
    for line in grammar:
        if finding_verb:
            if "verb = " in line:
                verb_words = re.findall(r'\"(\w+)\"', line)
                if len(verb_words) > 0:
                    main_verb = preprocessor(verb_words[0])
                    if main_verb:
                        synonyms = filter(None, map(preprocessor, verb_words[1:]))
                        intent_name = main_verb.capitalize() + "Intent"
                        slot_count = 0
                        utterances = set()
                        finding_verb = False
        else:
            for sentence in re.findall(r'"([\w\s]+)"', line):
                utterance, num_objects = parse_sentence(main_verb, sentence, preprocessor)
                utterances.update(generate_utterances(intent_name, utterance, main_verb, synonyms))
                slot_count = max(slot_count, num_objects)

            if line.strip() == "":
                intents.append(generate_intent(main_verb, intent_name, slot_count))
                all_utterances.update(utterances)
                finding_verb = True

    return {'intents': intents}, sorted(all_utterances)

def parse_words(zobjects, word_type, preprocessor):
    words = set()
    noun = re.compile(r'\[\s*\d+\]\s+@\s+\$[\da-f]+\s+(\w+).*\<' + word_type + '\>')
    for line in zobjects:
        m = noun.match(line)
        if m:
            words.add(m.group(1))
    return filter(None, map(preprocessor, words))

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

    grammar = invoke_infodump(args.infodump, "-w0", "-s", "-g", args.zfile)
    schema, utterances = parse_grammar(grammar, preprocessor)

    zobjects = invoke_infodump(args.infodump, "-w0", "-c1", "-d", args.zfile)
    nouns = parse_words(zobjects, 'noun', preprocessor)
    directions = parse_words(zobjects, 'dir', preprocessor)

    with open(args.schema, 'wt') as fp:
        json.dump(schema, fp, indent=2)

    dump_strings(utterances, args.utterances)
    dump_strings(nouns, args.nouns)
    dump_strings(directions, args.directions)
