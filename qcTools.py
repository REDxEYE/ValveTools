import re
from pathlib import Path


def get_mdl_from_qc(filepath):
    '''
    returns the path to the mdl generated by a given .qc file
    USAGE: pathToMdl = get_mdl_from_qc('%VCONTENT%/modname/models/assetname/scripts/assetModel.qc')
    '''
    filepath = Path(filepath)
    cmd = re.compile(r'^\$modelname\b')

    # get the mod the qc is under
    lines = filepath.read_text()
    if lines is None:
        return None

    for line in lines:
        line = line.strip().lower()
        if line.startswith('$modelname'):
            # now make sure the cmd token isn't a subset of $modelname
            if cmd.search(line):
                mdl = Path('%VGAME%/%VMOD%/models') / line.split()[1].replace('"', '')
                mdl = mdl.resolve()
                return mdl


def find_file(leaf_name, search_paths, base_paths):
    '''
    given a list of search_paths (qc style path extensions) and a list of base_paths (usually just
    derived using [Path('%VCONTENT%')/mod/'models' for mod in valve.GameInfo().getSearchMods()]
    and a filename, this method returns the actual path to the file
    '''
    paths = []
    for basePath in base_paths:
        for path in search_paths:
            theFile = basePath / path / leaf_name
            if theFile.exists:
                return theFile
            # print theFile
            paths.append(theFile)

    # if we haven't returned by now, see if we can find the file in perforce...
    return Path(leaf_name)


# def get_qc_includes(qcFilepath):
#     '''
#     returns a list of all .qci and .vta includes in a qc file...  everything else is too hard.  qc sucks
#     '''
#     qcFilepath = Path(qcFilepath)
#     vcontent = valve.content()
#     curPathRel = (qcFilepath.up() - '%VCONTENT%')[1:]
#     basePaths = [vcontent / mod / curPathRel for mod in valve.gameInfo.getSearchMods()]
#
#     searchStack = ['']
#     includes = []
#     visited = set()
#     inclRE = re.compile(r'^\$include\b')
#
#     def getIncludes(filepath, includes, visited):
#         # get the mod the qc is under
#         lines = filepath.read()
#         lines = valve.stripcomments(lines)
#         for line in lines:
#             line = line.strip().lower()
#             if line.startswith('$include'):
#                 if inclRE.search(line) is None: continue
#                 newFilepath = Path(line.split()[1].replace('"', '')).setExtension('qci')
#                 newFilepath = find_file(newFilepath, searchStack, basePaths)
#
#                 if not newFilepath.exists:
#                     print
#                     '### WARNING qcTools.get_qc_includes(): cannot find qci dependency:', newFilepath
#                     continue
#                 if newFilepath not in visited:
#                     includes.append(newFilepath)
#                     visited.add(newFilepath)
#                     getIncludes(newFilepath, includes, visited)
#             elif line.startswith('flexfile'):
#                 newFilepath = Path(line.split()[1].replace('"', '')).setExtension('vta')
#                 newFilepath = find_file(newFilepath, searchStack, basePaths)
#
#                 if not newFilepath.exists:
#                     print
#                     '### WARNING qcTools.get_qc_includes(): cannot find flex dependency:', newFilepath
#                     continue
#                 if newFilepath not in visited:
#                     includes.append(newFilepath)
#             elif line.startswith('$proceduralbones'):
#                 newFilepath = Path(line.split()[1].replace('"', '')).setExtension('vrd')
#                 newFilepath = find_file(newFilepath, searchStack, basePaths)
#
#                 if not newFilepath.exists:
#                     print
#                     '### WARNING qcTools.get_qc_includes(): cannot find vrd dependency:', newFilepath
#                     print
#                     searchStack, basePaths
#                     continue
#                 if newFilepath not in visited:
#                     includes.append(newFilepath)
#             elif line.startswith('$pushd'):
#                 searchStack.append(line.split()[1].replace('"', ''))
#             elif line.startswith('$popd'):
#                 searchStack.pop()
#                 pass
#             elif line.startswith('$addsearchdir'):
#                 searchStack.append(line.split()[1].replace('"', ''))
#
#     getIncludes(qcFilepath, includes, visited)
#
#     return includes

# end
