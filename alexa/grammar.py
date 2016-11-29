#!/usr/bin/python
# ZCode -> Alexa grammar generator
# Requires ztools 'infodump'

import sys, os, argparse, json, subprocess, re


def invoke_infodump(*args):
    cmd = " ".join(args)
    infodump = subprocess.Popen(args, stdout=subprocess.PIPE)
    return infodump.stdout.readlines()

def slot_name(index):
    return "Object%c" % (ord('A') + index,)

def parse_sentence(sentence):
    num_objects = 0
    words = []
    for word in sentence.split():
        if word == 'OBJ':
            word = '{%s}' % (slot_name(num_objects),)
            num_objects += 1
        words.append(word)
    return " ".join(words), num_objects

def generate_utterances(intent_name, sentence, main_verb, synonyms):
    yield intent_name + " " + sentence
    for synonym in synonyms:
        yield intent_name + " " + sentence.replace(main_verb, synonym)

def generate_intent(intent_name, slot_count):
    intent = {'intent': intent_name}
    if slot_count > 0:
        intent['slots'] = []
        for i in range(slot_count):
            intent['slots'].append({
                'name': slot_name(i),
                'type': 'LIST_OF_OBJECTS'
            })
    return intent

def parse_grammar(grammar):
    intents = []
    utterances = []
    finding_verb = True
    for line in grammar:
        if finding_verb:
            if "verb = " in line:
                verb_words = re.findall(r'\"(\w+)\"', line)
                if len(verb_words) > 0:
                    main_verb = verb_words[0]
                    synonyms = verb_words[1:]
                    intent_name = main_verb.capitalize() + "Intent"
                    slot_count = 0
                    finding_verb = False
        else:
            for sentence in re.findall(r'"([\w\s]+)"', line):
                utterance, num_objects = parse_sentence(sentence)
                utterances.extend(generate_utterances(intent_name, utterance, main_verb, synonyms))
                slot_count = max(slot_count, num_objects)

            if line.strip() == "":
                intents.append(generate_intent(intent_name, slot_count))
                finding_verb = True

    return {'intents': intents}, utterances

def parse_objects(zobjects):
    objects = set()
    noun = re.compile(r'\[\s*\d+\]\s+@\s+\$[\da-f]+\s+(\w+).*\<\w+\>')
    for line in zobjects:
        m = noun.match(line)
        if m:
            objects.add(m.group(1))
    return objects

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="zcode to alexa tool")
    parser.add_argument("--zfile", help="The zcode file to parse", required=True)
    parser.add_argument("--schema", help="The schema file to generate", default="intent-schema.txt")
    parser.add_argument("--objects", help="The object slot file to generate", default="objects.txt")
    parser.add_argument("--utterances", help="The utterances file to generate", default="utterances.txt")
    parser.add_argument("--infodump", help="Path to infodump tool", default="./infodump")
    args = parser.parse_args(sys.argv[1:])

    grammar = invoke_infodump(args.infodump, "-w0", "-s", "-g", args.zfile)
    schema, utterances = parse_grammar(grammar)

    zobjects = invoke_infodump(args.infodump, "-w0", "-c1", "-d", args.zfile)
    objects = parse_objects(zobjects)

    with open(args.schema, 'wt') as fp:
        json.dump(schema, fp, indent=2)

    with open(args.utterances, 'wt') as fp:
        fp.writelines(u + "\n" for u in utterances)

    with open(args.objects, 'wt') as fp:
        fp.writelines(o + "\n" for o in objects)
