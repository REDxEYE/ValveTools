import os
import string
from pathlib import Path

# from filesystem.path import Path
from pyparsing import *

ParserElement.enablePackrat()


class GoodException(Exception):
    """
    Good exceptions are just a general purpose way of breaking out of loops and whatnot. Basically anytime an exception is
    needed to control code flow and not indicate an actual problem using a GoodException makes it a little more obvious what
    the code is doing in the absence of comments
    """
    ...


BreakException = GoodException


def convert_to_float(s, len, toks):
    new = []
    for tok in toks:
        try:
            new.append(float(tok))
        except ValueError:
            new.append(tok)
    return new


def convert_to_int(s, len, toks):
    new = []
    for tok in toks: new.append(int(tok))
    return new


def convert_to_path(s, len, toks):
    new = []
    for tok in toks:
        new.append(QCDataPath(tok, '', g_searchPaths))

    return new


# define some common globals
g_digits = string.digits
g_alphas = string.ascii_letters
g_searchPaths = []

LPAREN, RPAREN, LBRACK, RBRACK, LBRACE, RBRACE = list(map(Suppress, "()[]{}"))

unquoted_path = Word(g_alphas + g_digits + '/\\_.')
quoted_path = Literal('"').suppress() + unquoted_path + Literal('"').suppress()
path = quoted_path | unquoted_path
path.setParseAction(convert_to_path)

unquoted_name = Word(g_alphas + g_digits + '.-_%')
quoted_name = Literal('"').suppress() + Word(g_alphas + g_digits + '.-_ ') + Literal('"').suppress()
name = quoted_name | unquoted_name

unquoted_float = Word(g_digits + '.-')
quoted_float = Literal('"').suppress() + unquoted_float + Literal('"').suppress()
float_word = quoted_float | unquoted_float
float_word.setParseAction(convert_to_float)

unquoted_int = Word(g_digits + '-')
quoted_int = Literal('"').suppress() + unquoted_int + Literal('"').suppress()
int_word = quoted_int | unquoted_int
int_word.setParseAction(convert_to_int)


def define_grammar():
    # first we define the basic grammar of a qc file
    unquoted_arg = Word(g_alphas + g_digits + '/\\_.-%@')
    quoted_arg = Suppress('"') + Optional(unquoted_arg) + Suppress('"')
    arg = unquoted_arg | quoted_arg

    # define variable arguments - a variable argument is a normal argument with an embedded variable (ie $str$)
    unquoted_variable = Combine(
        Optional(unquoted_arg) + Literal('$') + Word(g_alphas + g_digits + '_') + Literal('$') + Optional(unquoted_arg))
    quoted_variable = Suppress('"') + unquoted_variable + Suppress('"')
    variable = unquoted_variable | quoted_variable

    cmd_token = variable | arg
    dollar_arg = Suppress('$') + Word(g_alphas, g_alphas + g_digits + '_')
    block_cmd_token = variable | arg | dollar_arg

    # define the recursive braces that can be optionally attached to a qc command
    block_args = Forward()
    block_args << (block_cmd_token | (LBRACE + ZeroOrMore(block_args) + RBRACE))

    # finally construct the actuall generic qc command grammar
    cmd_def = Suppress('$') + Word(g_alphas, g_alphas + g_digits + '_')
    qc_grammar = cmd_def + ZeroOrMore(cmd_token) + Optional(LBRACE + OneOrMore(block_args) + RBRACE)
    qc_grammar.ignore(dblSlashComment)
    qc_grammar.ignore(cStyleComment)

    return qc_grammar


g_extensions = ['smd', 'dmx', 'qci', 'vrd', 'vta']


class QCDataPath:
    """
    qc Path objects contain a base path, a path modifier and can be easily transformed
    into fully qualified paths
    """

    def __init__(self, path, base_path, search_paths=None):
        if search_paths is None:
            search_paths = []
        self.path = path
        self.base = base_path
        self.fullpath = None

        # resolve the path
        search_paths = list(map(Path, [base_path] + search_paths + g_searchPaths))
        try:
            for p in search_paths:
                for ext in g_extensions:
                    try_path = p / path  # type: Path
                    try_path = try_path.with_suffix('.' + ext)
                    if try_path.exists:
                        self.fullpath = try_path
                        raise BreakException
        except BreakException:
            pass

    def __str__(self):
        return self.path


class QCData:
    """
    creates an object containing all qc commands in a given file - it collapses includes
    and resolves variables, as well as turning paths into Path objects
    """

    def __init__(self, filepath, depth=0):
        """
        bleh
        """
        # create instance attributes
        self.path, self.file = os.path.split(filepath)
        self.filepath = Path(filepath)
        self.includes = {}
        self.variables = {}
        self.macros = []
        self.cmdlists = []
        self.cmds = []
        self.paths = g_searchPaths

        self.paths.append(str(Path(filepath).parent))

        # open the file and read it in if the file exists
        if not os.path.exists(filepath):
            return

        tmp_file_obj = open(filepath)
        contents_str = tmp_file_obj.read().replace('\t', ' ')
        tmp_file_obj.close()

        contents_iter = define_grammar().scanString(contents_str)
        base_qc_path = os.path.dirname(filepath)

        while True:
            try:
                cur = next(contents_iter)
                results = cur[0].asList()
                cmd = results[0].lower()

                if cmd == 'include':
                    # we still need to create an include qc cmd object to preserve file nesting
                    raw_parse = contents_str[cur[1]:cur[2]].strip()
                    include_cmd_obj = Include(results, raw_parse, self)
                    self.cmds.append(include_cmd_obj)

                    # recurse into the include file
                    include_file = Path(include_cmd_obj.file.fullpath)

                    if include_file.exists:
                        print('\t' * depth + 'PARSING FILE: %s' % include_file)
                        include_obj = QCData(include_file, depth + 1)
                        self.includes[include_file] = include_obj
                    else:
                        print('cannot find file:', results[1])
                        print()
                elif cmd == 'definemacro':
                    # so now we want to keep iterating until we hit the end of the macro definition - which is a bit weird...
                    # macros don't really have a "block" to define the macro commands, they merely have a weird endline
                    # character, so we need to parse line by line until we stop seeing the endline character.  creating a
                    # grammar to deal with this is just plain tricky, which is why the parsing is broken up into these steps
                    start = cur[1]
                    end = cur[2]
                    tmp_str = contents_str[end:]
                    macro_end = get_line(tmp_str)
                    macro_contents = tmp_str[:macro_end]
                    while macro_contents.strip()[-2:] == '\\\\':
                        macro_end = get_line(tmp_str, macro_end + 1)  # add one because the last ending was at a newline
                        macro_contents = tmp_str[:macro_end]

                    # replace the current stack item with the new string, and rebuild the iterator
                    contents_str = tmp_str[macro_end:]
                    contents_iter = define_grammar().scanString(contents_str)

                    # once we have all the macro data, we need to parse it (again) into proper structures to build an object
                    # for it, and then re-setup the parse iterator with the resulting string
                    macro_obj = Macro(results, macro_contents, self)
                    self.macros.append(macro_obj)
                    self.cmds.append(macro_obj)
                elif cmd == 'definevariable':
                    # the variable assignment may be a variable itself, so we need to resolve it
                    name = results[1]
                    value = results[2]
                    value = self.resolve_variables([value])[0]
                    variable_obj = Variable(name, value)
                    self.variables[results[1]] = variable_obj
                    self.cmds.append(variable_obj)
                    print('defining variable $%s$ = "%s"' % (name, value))
                elif cmd == 'cmdlist':
                    cmdlist_obj = Cmdlist(results, contents_str[cur[1]:cur[2]].strip(), self)
                    self.cmdlists.append(cmdlist_obj)
                    self.cmds.append(cmdlist_obj)
                elif cmd == 'append':
                    # so the append command allows the addition of args to a sequence object
                    sequence_to_append = results[1].lower()
                    extra_cmds = results[2:]

                    for cmd_obj in self.cmds:
                        if isinstance(cmd_obj, Sequence):
                            if cmd_obj.name.lower() == sequence_to_append:
                                cmd_obj.append(extra_cmds)
                elif cmd == 'pushd':
                    path_name = results[1]
                    path_name = self.resolve_variables([path_name])[0]
                    self.paths.append('%s/%s' % (self.path, path_name))
                elif cmd == 'popd':
                    self.paths.pop()
                elif cmd == 'addsearchdir':
                    g_searchPaths.append('%s/%s' % (self.path, cur[0][1]))
                else:
                    raw_parse = contents_str[cur[1]:cur[2]].strip()
                    try:
                        results = self.resolve_variables(results)
                        type_obj = globals()[cmd.capitalize()]
                        cmd_obj = type_obj(results, raw_parse, self)
                        self.cmds.append(cmd_obj)
                    except NameError:
                        # if this is the case then the "command" may actually be a macro - so deal with it...
                        macro_name = results[0]
                        collapsed_macro = self.get_macro(macro_name, results[1:])
                        self.cmds.extend(collapsed_macro)

            except StopIteration:
                print('\t' * (depth - 1), 'END OF FILE', filepath)
                print()
                break

    def to_string(self, collapse=False):
        lines = []
        for cmd in self.cmds:
            if collapse and isinstance(cmd, Include):
                qcFilepath = Path(cmd.file.fullpath)
                lines.append('/* --------- below is the contents of %s --------- */\n' % qcFilepath)
                qcFileObj = self.includes[qcFilepath]
                lines.append(qcFileObj.to_string(collapse))
            else:
                lines.append(str(cmd))

        return '\n'.join(lines)

    __str__ = to_string

    def has_macro(self, macro_name):
        """if the given macro name doesn't exist in the list then this returns null"""
        for m in self.macros:
            if m.name == macro_name: return m

        return None

    def get_macro(self, macro_name, macro_arg_list):
        the_macro = self.has_macro(macro_name)
        if the_macro is None:
            msg_str = '###ERROR: the macro named: ' + macro_name + ' doesnt exist'
            print(msg_str)
            raise Exception(msg_str)

        return the_macro.collapse(macro_arg_list)

    def resolve_variables(self, tokens):
        for n in range(len(tokens)):
            if tokens[n].count('$') == 2:
                variable_name_start = tokens[n].find('$')
                variable_name_end = tokens[n].find('$', variable_name_start + 1)
                variable_name = tokens[n][variable_name_start:variable_name_end + 1]
                try:
                    without_dollars = variable_name[1:-1]
                    variable_value = self.variables[without_dollars].value
                    tokens[n] = tokens[n].replace(variable_name, variable_value)
                except KeyError:
                    raise KeyError("the %s variable hasn't been defined!" % (variable_name,))
        return tokens

    def serialize(self):
        return '//\n//This .qc was auto-generated by the qcParser...  YMMV!\n\n//\n%s' % self.to_string()

    def write(self, filepath=None):
        """
        writes all files back to disk
        """
        if filepath is None:
            filepath = self.filepath
        else:
            filepath = Path(filepath)
        os.makedirs(filepath.parent, exist_ok=True)
        with filepath.open('w') as fp:
            fp.write(self.serialize())

    def collapse(self, filepath=None):
        """
        collapses a qc into a single file
        """
        if filepath is None:
            filepath = self.filepath
        else:
            filepath = Path(filepath)
        os.makedirs(filepath.parent, exist_ok=True)
        with filepath.open('w') as fp:
            fp.write(self.to_string(True))  # disable auto-add to p4...

    def iter_dependent_cmds(self):
        for cmd in self.cmds:
            if isinstance(cmd, CommandWithExternalDependency):
                yield cmd

    def iter_dependencies(self):
        visited = set()
        for cmd in self.cmds:
            if isinstance(cmd, CommandWithExternalDependency):
                if cmd not in visited:
                    visited.add(cmd)

                    f = cmd.file
                    if f is None:
                        continue

                    yield f

    def get_dependencies(self):
        dependencies = set()
        for cmd in self.iter_dependent_cmds():
            if cmd.file is None:
                continue

            dependencies.add(cmd.file.fullpath)

        return list(sorted(dependencies))

    def get_modelname(self):
        for cmd in self.cmds:
            if isinstance(cmd, Modelname):
                return cmd.name

    def set_modelname(self, newModelname):
        for cmd in self.cmds:
            if isinstance(cmd, Modelname):
                cmd.name = newModelname
                return True

        return False

    def get_mdl_filepath(self):
        return Path(self.get_modelname()).parent


def get_line(the_string, start_pos=0):
    n = start_pos
    while the_string[n] != '\n':
        n += 1

    return n


def get_token(string_iterator):
    token = ''
    non_token_chars = ' \n'
    while True:
        try:
            char = next(string_iterator)
            if char not in non_token_chars:
                token += char
            else:
                return token
        except StopIteration:
            return token


def collapse_cmd_list(arg_list, list_of_cmdlists):
    arg_iter = iter(arg_list)
    n = -1
    while True:
        try:
            tok = next(arg_iter)
            n += 1
            if isinstance(tok, str):
                if tok.lower() == 'cmdlist':
                    cmdlist_name = next(arg_iter)
                    for cmdlist in list_of_cmdlists:
                        if cmdlist.name == cmdlist_name:
                            arg_list.pop(n)  # remove the cmdlist arg
                            arg_list.pop(n)  # remove the cmdlist name arg
                            arg_list = arg_list[:n] + cmdlist.args + arg_list[n:]

                            # reset the iterator
                            arg_iter = iter(arg_list)
                            n = -1
        except StopIteration:
            return arg_list


class Macro:
    """
    class to deal with macro definitions
    """

    def __init__(self, macro_toks, macro_contents, from_qc_source):
        """
        the macro tokens are the list of variable names for the macro, while the macro_contents is the actual
        content string for the parsed macro - so we need to parse it here to define the actual cmd objects to
        return
        """
        self.cmd = 'definemacro'  # just for consistency
        self.name = macro_toks[1]
        self.source = from_qc_source
        self.raw = macro_contents
        self.variables = []
        self.cmds = []

        # validate the list of variables passed in - we need to make sure there are no \\ symbols
        for tok in macro_toks[2:]:
            if tok != '\\\\': self.variables.append(tok)

        macro_contents = macro_contents.replace('\\\\', '')
        macro_grammar_iter = define_grammar().scanString(macro_contents)

        # remove the continue keyword from the list of cmds
        while True:
            try:
                cmd_tokens, start, end = next(macro_grammar_iter)
                self.cmds.append(cmd_tokens.asList())

            except StopIteration:
                break

    def collapse(self, macro_arg_list):
        """
        will return a list of the qc commands this macro defines with all variables resolved
        """
        if len(macro_arg_list) != len(self.variables):
            msg = self.name, "macro variable list donesn't match:", len(macro_arg_list), len(self.variables)
            print(msg)
            raise Exception(msg)

        cmd_objs = []
        for n in range(len(self.cmds)):
            cmd_args = list(self.cmds[n])  # make a copy of the cmd list
            for i in range(len(self.cmds[n])):
                for j in range(len(self.variables)):
                    # iterate over all variable values to produce the tokens and return the appropriate qc object
                    cmd_args[i] = cmd_args[i].replace('$' + self.variables[j] + '$', str(macro_arg_list[j]))

            try:
                cmd_as_str = ' '.join(cmd_args[1:])
                cmd_obj = eval(cmd_args[0][0].upper() + cmd_args[0][1:].lower() + "(cmd_args,cmd_as_str,self.source)")
                cmd_objs.append(cmd_obj)
            except NameError:
                # if we get a NameError here then there is a good chance the macro contains another macro - so we need to resolve it
                cmd_name = cmd_args[0]
                matched = False
                for macro in self.source.macros:
                    if macro.name == cmd_name:
                        cmd_obj = macro.collapse(cmd_args[1:], self.source.macros)
                        cmd_objs.append(cmd_obj)
                        matched = True
                        break

                if not matched:
                    raise NameError(cmd_name)

        return cmd_objs


class Variable:
    def __init__(self, name, value):
        self.name = name
        self.value = value

    def __str__(self):
        return '$definevariable %s="%s"' % (self.name, self.value)


class InvalidCommand(Exception):
    def __init__(self, cmd_toks):
        self.cmdToks = cmd_toks

    def __str__(self):
        as_str = self.cmdToks[0] + 'has no processing class'


class TokenCountException(Exception):
    def __init__(self, command_name, expected_token_count, actual_token_count):
        self.name = command_name
        self.expectedCount = expected_token_count
        self.actualCount = actual_token_count

    def __str__(self):
        exception_str = 'the %s command expected %d tokens, but only found %d', (
            self.name, self.expectedCount, self.actualCount)
        return exception_str


class SequenceException(Exception):
    def __init__(self):
        pass

    def __str__(self):
        return 'sequence error'


class Command(object):
    """
    super class for all qc command classes - provides super basic common functionality
    it builds the common attributes, and provides a consistent way of parsing a list of
    grammar definitions for more complex commands (sequence/animation commands, and a few
    others) but for the most part, each class should probably handle its own parsing of
    the tokens passed to it
    """

    def __init__(self, cmd_toks, raw_parse, from_qc_source, to_raw_start=1):
        """
        from_qc_source is the QCData object that this object was created by and lives in.  it is not uncommon
        for a qc command object to need to query more global information about the qc file
        """
        self.cmd = cmd_toks[0].lower()
        self.args = cmd_toks[to_raw_start:]

        # collapse any cmdlist args
        cmds_that_have_cmdlists = ['sequence', 'animation']
        if self.cmd in cmds_that_have_cmdlists:
            self.args = collapse_cmd_list(self.args, from_qc_source.cmdlists)

        # store the raw parse string, and the source qc file object reference
        self.raw = raw_parse
        self.source = from_qc_source
        self.unparsed = ''

    def __str__(self):
        # asStr = "$%s %s\n" % (self.cmd, " ".join( str( a ) for a in self.args ))

        return self.raw

    def __repr__(self):
        return self.__str__()

    def parse_grammar_list(self, cmd_options, args_to_parse=None):
        """
        this is for more cmd objects that have more complicated grammars - like the sequence command.  given a list of grammars, this
        method will iterate over the arguments and parse them into groups.  for example the sequence command has a bunch of modifier
        args that have their own structures - this will gather all the options into sublists and put them back in the args attribute
        """
        if args_to_parse == None:
            args_to_parse = self.args

        n = 0
        temp_str = ' '.join(args_to_parse)
        self.args = []
        parse_iteration_limit = 250
        for cmd_opt in cmd_options:
            while True:
                n += 1
                results = cmd_opt.scanString(temp_str, 1)
                if n > parse_iteration_limit:
                    print('the %s command has gone over the parse iteration limit\n%s' % (self.cmd, self.raw))
                    return
                try:
                    result = next(results)
                    self.args.append(result[0].asList())
                    temp_str = (temp_str[:result[1]] + temp_str[result[2]:]).strip()
                except StopIteration:
                    break

        if len(temp_str.strip()):
            self.unparsed = temp_str.strip()
            print(
                "WARNING:  there were some tokens left on this command (%s) that I didn't know how to parse..." % self.unparsed)


class CommandWithExternalDependency(Command):
    def __init__(self, filepath_idx, cmd_toks, raw_parse, from_qc_source, to_raw_start=1):
        Command.__init__(self, cmd_toks, raw_parse, from_qc_source, to_raw_start)
        self.file = QCDataPath(cmd_toks[filepath_idx], from_qc_source.path, from_qc_source.paths)

    def set_dependency(self, new_dependency):
        self.file = new_dependency


class Alwayscollapse(Command): pass


class Ambientboost(Command): pass


class Animblocksize(Command): pass


class Attachment(Command): pass


class Bbox(Command):
    EXPECTED_TOKEN_COUNT = 7

    def __init__(self, cmd_toks, raw_parse, from_qc_source):
        if len(cmd_toks) != self.EXPECTED_TOKEN_COUNT:
            raise TokenCountException('$bbox', self.EXPECTED_TOKEN_COUNT, len(cmd_toks))

        self.min = (float(cmd_toks[1]), float(cmd_toks[2]), float(cmd_toks[3]))
        self.max = (float(cmd_toks[4]), float(cmd_toks[5]), float(cmd_toks[6]))
        Command.__init__(self, cmd_toks, raw_parse, from_qc_source)

    def __str__(self):
        as_str = '$' + self.cmd + ' %.6f %.6f %.6f %.6f %.6f %.6f' % (
            self.min[0], self.min[1], self.min[2], self.max[0], self.max[1], self.max[2])
        return as_str


class Body(CommandWithExternalDependency):
    EXPECTED_TOKEN_COUNT = 3

    def __init__(self, cmd_toks, raw_parse, from_qc_source):
        if len(cmd_toks) != self.EXPECTED_TOKEN_COUNT:
            raise TokenCountException('$body', self.EXPECTED_TOKEN_COUNT, len(cmd_toks))

        CommandWithExternalDependency.__init__(self, 2, cmd_toks, raw_parse, from_qc_source)

        self.name = cmd_toks[1]

    def __str__(self):
        return '$body "%s" "%s"' % (self.name, self.file)


class Bodygroup(Command):
    def __init__(self, cmd_toks, raw_parse, from_qc_source):
        Command.__init__(self, cmd_toks, raw_parse, from_qc_source)

    def __str__(self):
        args = self.args.copy()
        as_str = '${} "{}"'.format(self.cmd, args.pop(0)) + ' \n{\n\t'
        while args:
            arg = args.pop(0)
            if arg == 'blank':
                as_str += arg
            elif arg == 'studio':
                as_str += '{} "{}"'.format(arg, args.pop(0))
            else:
                as_str += arg
            if len(args) > 0:
                as_str += '\n\t'
        as_str += '\r}'
        return as_str


class Bonemerge(Command): pass


class Bonesaveframe(Command): pass


class Calctransitions(Command): pass


class Casttextureshadows(Command): pass


class Cbox(Command): pass


class Cdmaterials(Command):
    def __str__(self):
        return '\n$%s "%s"' % (self.cmd, self.args[0])


class Centerbonesonverts(Command): pass


class Clampworldspace(Command): pass


class Cliptotextures(Command): pass


class Cmdlist(Command):
    """
    cmdlists are basically named collections of commonly used arguments and can be recalled by using the 'cmdlist' argument
    """

    def __init__(self, cmd_toks, raw_parse, from_qc_source):
        self.name = cmd_toks[1]
        Command.__init__(self, cmd_toks, raw_parse, from_qc_source, 2)

    def __str__(self):
        toStr = '$cmdlist "%s" { %s }' % (self.name, ' '.join(self.args))

        return toStr


class Collapsebones(Command): pass


class Collapsebonesaggressive(Command): pass


class Collisionjoints(Command): pass


class Collisionmodel(CommandWithExternalDependency):
    as_str_lookup = {
        'mass': ('$%s %s', 1),
        'automass': ('$%s', 0),
        'concave': ('$%s', 0),
        'maxconvexpieces': ('$%s %s', 1),
        'masscenter': ('$%s %s %s %s', 3),
        'inertia': ('$%s %s', 1),
        'damping': ('$%s %s', 1),
        'rotdamping': ('$%s %s', 1),
        'drag': ('$%s %s', 1),
        'remove2d': ('$%s', 0),
        'assumeworldspace': ('$%s', 0)
    }

    def __init__(self, cmd_toks, raw_parse, from_qc_source):
        CommandWithExternalDependency.__init__(self, 1, cmd_toks, raw_parse, from_qc_source)

    def __str__(self):
        arg_toks = []
        arg_iter = iter(self.args[1:])
        while True:
            try:
                arg = next(arg_iter)
                format_str, num_toks = self.as_str_lookup[arg.lower()]
                format_args = (arg,) + tuple(next(arg_iter) for n in range(num_toks))
                arg_toks.append(format_str % format_args)
            except StopIteration:
                break

        args_as_strs = ''
        if arg_toks:
            args_as_strs = '{\n\t%s\n\t}' % '\n\t'.join(arg_toks)

        return '$collisionmodel "%s" %s' % (self.file, args_as_strs)


class Collisiontext(Command): pass


class Constantdirectionallight(Command): pass


class Contents(Command): pass


class Controller(Command): pass


class Declareanimation(Command): pass


class Declaresequence(Command): pass


class Defaultweightlist(Command): pass


class Definebone(Command):
    EXPECTED_TOKEN_COUNT = 12

    def __init__(self, cmd_toks, raw_parse, from_qc_source):

        if len(cmd_toks) != self.EXPECTED_TOKEN_COUNT:
            # if the length isn't as expected, pad the end out with zeroes
            while len(cmd_toks) < self.EXPECTED_TOKEN_COUNT:
                cmd_toks.append('0')

        self.bone = cmd_toks[1]
        self.parent = cmd_toks[2]
        self.pos = list(map(float, (cmd_toks[3], cmd_toks[4], cmd_toks[5])))
        self.rot = list(map(float, (cmd_toks[6], cmd_toks[7], cmd_toks[8])))
        self.fix = list(map(float, (cmd_toks[9], cmd_toks[10], cmd_toks[11])))
        Command.__init__(self, cmd_toks, raw_parse, from_qc_source)

    def __str__(self):
        as_str = '$%s "%s" "%s" ' % (self.cmd, self.bone, self.parent)
        as_str += '%6f %6f %6f ' % (self.pos[0], self.pos[1], self.pos[2])
        as_str += '%4f %4f %4f ' % (self.rot[0], self.rot[1], self.rot[2])
        as_str += '%4f %4f %4f ' % (self.fix[0], self.fix[1], self.fix[2])
        as_str = self.raw
        return as_str


class Donotcastshadows(Command): pass


class Externaltextures(Command): pass


class Eyeposition(Command): pass


class Fakevta(Command): pass


class Forcecapsules(Command): pass


class Forcephonemecrossfade(Command): pass


class Forcerealign(Command): pass


class Gamma(Command): pass


class Hbox(Command): pass


class Hboxset(Command): pass


class Heirarchy(Command): pass


class Hgroup(Command): pass


class Hierarchy(Command): pass


class Ikautoplaylock(Command): pass


class Ikchain(Command): pass


class Illumposition(Command): pass


class Include(CommandWithExternalDependency):
    def __init__(self, cmd_toks, raw_parse, from_qc_source, to_raw_start=1):
        CommandWithExternalDependency.__init__(self, 1, cmd_toks, raw_parse, from_qc_source, to_raw_start=1)

    def __str__(self):
        return '$include "%s"' % self.args[0]


class Includemodel(Command): pass


class Insertbone(Command): pass


class Jigglebone(Command): pass


class Jointcontents(Command): pass


class Jointsurfaceprop(Command): pass


class Keyvalues(Command): pass


class Limitrotation(Command): pass


class Lockbonelengths(Command): pass


class Lod(Command):
    pass


# def __str__( self ):
# return '//no lod support yet...'


class Maxeyedeflection(Command): pass


class Minlod(Command): pass


class Model(CommandWithExternalDependency):
    def __init__(self, cmd_toks, raw_parse, from_qc_source):
        CommandWithExternalDependency.__init__(self, 2, cmd_toks, raw_parse, from_qc_source, 3)

        self.name = cmd_toks[1]

        self.parse_raw()

    # def __str__( self ):
    # return '$%s "%s" "%s"\n' % (self.cmd, self.name, self.file)
    def parse_raw(self):
        cmd_options = []

        cmd_options.append(CaselessKeyword(
            'eyeball') + name + name + float_word + float_word + float_word + name + float_word + float_word + name + float_word)

        # construct the eyelid grammar
        lowr_opt = CaselessKeyword('lowerer') + int_word + float_word
        neut_opt = CaselessKeyword('neutral') + int_word + float_word
        rais_opt = CaselessKeyword('raiser') + int_word + float_word
        cmd_options.append(
            CaselessKeyword('eyelid') + name + path + ZeroOrMore(lowr_opt | neut_opt | rais_opt) + CaselessKeyword(
                'split') + float_word + CaselessKeyword('eyeball') + name)

        cmd_options.append(CaselessKeyword('mouth') + name + name)
        cmd_options.append(CaselessKeyword('spherenormals') + name + float_word + float_word + float_word)
        Command.parse_grammar_list(self, cmd_options)


class Modelname(Command):
    def __init__(self, cmd_toks, raw_parse, from_qc_source):
        self.name = cmd_toks[1]
        Command.__init__(self, cmd_toks, raw_parse, from_qc_source)

    def __str__(self):
        return '$%s "%s"' % (self.cmd, self.name)


class Mostlyopaque(Command): pass


class Motionrollback(Command): pass


class Noforcedfade(Command): pass


class Obsolete(Command): pass


class Opaque(Command): pass


class Origin(Command): pass


class Poseparameter(Command): pass


class Physreduce(Command): pass


class Prepend(Command): pass


class Proceduralbones(Command): pass


class Realignbones(Command): pass


class Renamebone(Command): pass


class Renamematerial(Command): pass


class Root(Command): pass


class Scale(Command):
    def __init__(self, cmd_toks, raw_parse, from_qc_source):
        Command.__init__(self, cmd_toks, raw_parse, from_qc_source, 1)
        self.args[0] = float(cmd_toks[1])

    def __str__(self):
        return '$scale %s' % self.args[0]


class Screenalign(Command): pass


class Sectionframes(Command): pass


class Sequence(CommandWithExternalDependency):
    def __init__(self, cmd_toks, raw_parse, from_qc_source):
        if len(cmd_toks) < 2:
            # need some custom exceptions here so we can error intelligently.  in this case we want
            # the user to know that there aren't enough args passed to this particular sequence cmd
            raise SequenceException()

        # init the parent class
        CommandWithExternalDependency.__init__(self, 2, cmd_toks, raw_parse, from_qc_source, 3)

        self.name = cmd_toks[1]

        grammar = self.define_grammar()
        Command.parse_grammar_list(self, grammar)

    def __str__(self):
        as_str = '$%s "%s" "%s"' % (self.cmd, self.name, self.file)
        if len(self.args):
            as_str += ' {\n'

        for data in self.args:
            as_str += '\t%s\n' % ' '.join(str(d) for d in data)

        try:
            if len(self.unparsed):
                as_str += '\n\t//WARNING :: THE FOLLOWING WAS UNPARSED\n'
                as_str += '\t{' + self.unparsed + '}\n'
        except AttributeError:
            pass

        if len(self.args):
            as_str += '\t}\n'

        return as_str

    def define_grammar(self):
        """
        defines the grammar for the qc command and also kicks off the parser for the
        command tokens
        """
        cmd_options = []

        cmd_options.append(Optional(CaselessKeyword('activity')) + Regex('^(act_|ACT_)[a-zA-Z_0-9]+') + float_word)

        cmd_options.append(CaselessKeyword('activity') + name + int_word)
        cmd_options.append(CaselessKeyword('alignto') + name)
        cmd_options.append(CaselessKeyword('autoplay'))
        cmd_options.append(CaselessKeyword('addlayer') + path)

        # define blendlayer
        blend_layer_opts = []
        blend_layer_opts.append(CaselessKeyword('spline'))
        blend_layer_opts.append(CaselessKeyword('xfade'))
        blend_layer_opts.append(CaselessKeyword('noblend'))
        blend_layer_opts.append(CaselessKeyword('poseparameter') + name)
        blend_layer_opts.append(CaselessKeyword('local'))
        temp_str = 'ZeroOrMore('
        for n in range(len(blend_layer_opts)):
            temp_str += 'blend_layer_opts[' + str(n) + '] |'
        temp_str = temp_str[:-1] + ')'
        blend_layer_opts_grammar = eval(temp_str)
        cmd_options.append(CaselessKeyword(
            'blendlayer') + path + float_word + float_word + float_word + float_word + blend_layer_opts_grammar)

        cmd_options.append(CaselessKeyword('blendwidth') + int_word)
        cmd_options.append(CaselessKeyword('blend') + float_word + float_word)
        cmd_options.append(CaselessKeyword('blockname') + name)
        cmd_options.append(CaselessKeyword('calcblend') + name + name + Word('xyz'))
        cmd_options.append(CaselessKeyword('delta'))
        cmd_options.append(CaselessKeyword('event') + name + int_word + name)
        cmd_options.append(CaselessKeyword('fadein') + float_word)
        cmd_options.append(CaselessKeyword('fadeout') + float_word)
        cmd_options.append(CaselessKeyword('fps') + float_word)
        cmd_options.append(CaselessKeyword('frame') + int_word + int_word)
        cmd_options.append(CaselessKeyword('frames') + int_word + int_word)
        cmd_options.append(CaselessKeyword('hidden'))

        # define ikrule
        ikrule_opts = []
        ikrule_opts.append(CaselessKeyword('attachment') + name)
        ikrule_opts.append(CaselessKeyword('bone'))
        ikrule_opts.append(CaselessKeyword('contact') + int_word)
        ikrule_opts.append(CaselessKeyword('fakeorigin') + float_word + float_word + float_word)
        ikrule_opts.append(CaselessKeyword('fakerotate') + float_word + float_word + float_word)
        ikrule_opts.append(CaselessKeyword('floor') + float_word)
        ikrule_opts.append(CaselessKeyword('footstep'))
        ikrule_opts.append(CaselessKeyword('height') + float_word)
        ikrule_opts.append(CaselessKeyword('pad') + float_word)
        ikrule_opts.append(CaselessKeyword('radius') + float_word)
        ikrule_opts.append(CaselessKeyword('range') + float_word + float_word + float_word + float_word)
        ikrule_opts.append(CaselessKeyword('release'))
        ikrule_opts.append(CaselessKeyword('target') + int_word)
        ikrule_opts.append(CaselessKeyword('touch') + name)
        ikrule_opts.append(CaselessKeyword('unlatch'))
        ikrule_opts.append(CaselessKeyword('usesequence'))
        ikrule_opts.append(CaselessKeyword('usesource'))
        temp_str = 'OneOrMore('
        for n in range(len(ikrule_opts)):
            temp_str += 'ikrule_opts[' + str(n) + '] |'
        temp_str = temp_str[:-1] + ')'
        ikrule_opts_grammar = eval(temp_str)
        cmd_options.append(CaselessKeyword('ikrule') + name + ikrule_opts_grammar)

        cmd_options.append(CaselessKeyword('iklock') + name + float_word + float_word)
        cmd_options.append(CaselessKeyword('loop'))
        cmd_options.append(CaselessKeyword('motionrollback'))
        cmd_options.append(CaselessKeyword('noanimblockstall'))
        cmd_options.append(CaselessKeyword('noanimation'))
        cmd_options.append(CaselessKeyword('noanimblock'))
        cmd_options.append(CaselessKeyword('noautoik'))
        cmd_options.append(CaselessKeyword('autoik'))
        cmd_options.append(CaselessKeyword('numframes') + int_word)
        cmd_options.append(CaselessKeyword('predelta'))
        cmd_options.append(CaselessKeyword('presubtract') + name + int_word)
        cmd_options.append(CaselessKeyword('realtime'))
        cmd_options.append(CaselessKeyword('rotateto') + float_word)
        cmd_options.append(CaselessKeyword('scale') + float_word)
        cmd_options.append(CaselessKeyword('snap'))
        cmd_options.append(CaselessKeyword('startloop') + name)
        cmd_options.append(CaselessKeyword('subtract') + name + int_word)
        cmd_options.append(CaselessKeyword('weightlist') + name)
        cmd_options.append(CaselessKeyword('worldspaceblendloop') + name + int_word)

        # motionType keywords
        cmd_options.append(CaselessKeyword('LXR'))
        cmd_options.append(CaselessKeyword('LYR'))
        cmd_options.append(CaselessKeyword('LZR'))
        cmd_options.append(CaselessKeyword('XR'))
        cmd_options.append(CaselessKeyword('YR'))
        cmd_options.append(CaselessKeyword('ZR'))
        cmd_options.append(CaselessKeyword('LX'))
        cmd_options.append(CaselessKeyword('LY'))
        cmd_options.append(CaselessKeyword('LZ'))
        cmd_options.append(CaselessKeyword('LM'))
        cmd_options.append(CaselessKeyword('LQ'))
        cmd_options.append(CaselessKeyword('X'))
        cmd_options.append(CaselessKeyword('Y'))
        cmd_options.append(CaselessKeyword('Z'))

        return cmd_options

    def append(self, toks):
        args = self.args + toks
        toks = collapse_cmd_list(toks, self.source.cmdlists)
        grammar = self.define_grammar()
        Command.parse_grammar_list(self, grammar, toks)
        self.args = args


class Animation(Sequence): pass


class Shadowlod(Command): pass


class Skipboneinbbox(Command): pass


class Skiptransition(Command): pass


class Staticprop(Command):
    def __str__(self):
        return '$staticprop\n'


class Surfaceprop(Command):
    def __str__(self):
        return '$surfaceprop "%s"\n' % self.args[0]


class Texturegroup(Command):
    # def __init__( self, cmdToks, rawParse, fromQCSource ):
    # self.name = cmdToks[ 1 ]
    # Command.__init__( self, cmdToks, rawParse, fromQCSource, 2 )
    def __str__(self):
        # return '$%s "%s" {\n\t%s\n\t}\n' % (self.cmd, self.name, '\n\t'.join( '"%s"' % a for a in self.args ))
        return self.raw


class Unlockdefinebones(Command): pass


class Upaxis(Command): pass


class Zbrush(Command): pass


class Weightlist(Command):
    def __init__(self, cmd_toks, raw_parse, from_qc_source):
        if len(cmd_toks) < 4:
            raise AttributeError

        self.name = cmd_toks[1]
        self.weights = []
        Command.__init__(self, cmd_toks, raw_parse, from_qc_source, 2)

    def __str__(self):
        as_str = '$weightlist "' + self.name + '" {\n'
        arg_iter = iter(self.args)
        try:
            while True:
                as_str += '\t"' + next(arg_iter) + '" ' + next(arg_iter) + '\n'
        except StopIteration:
            pass
        as_str += '}'

        return as_str


# end
if __name__ == '__main__':
    qc = QCData(r"E:\SOURCE_ENGINE_MODEL_SOURCES\Mathew_Kelly\mathew_kelly.qc")
    qc.write('./test.qc')
    ...
