from ValveUtils import Chunk, KeyValueFile


def parse_vmf_line(line):
    """
    super dumb proc - basically it only works for fairly cleanly structured vmf files - which is fine because
    they're almost never hand edited...  works only if there is a quoted key value pair: "key" "value" OR
    an unquoted named compound: compoundkey
    """
    if line.startswith('"'):  # == 4:
        toks = line[1:-1].split('"')
        return toks[0], toks[-1]
    return line, []


class VmfChunk(Chunk):
    def __init__(self, key, value=None, parent=None):
        Chunk.__init__(self, key, value, parent)

    def get_inputs_and_outputs(self, vmfFile=None):
        """
        will return a tuple containing inputs, outputs - each a list containing the chunks that either refer
        to this chunk (inputs), or are referred to by it (outputs)
        """
        if vmfFile is None:
            vmfFile = self.getFileObject()

        inputs = []
        try:
            this_entity_name = self.targetname.value
            all_connection_chunks = vmfFile.findKey('connections')
            for connectionChunk in all_connection_chunks:
                for connection in connectionChunk:
                    entity_name = connection.value.split(',')[0]
                    if entity_name == this_entity_name:
                        # the connection chunk is literally just the chunk containing the connection data - which lives
                        # inside a connections chunk, which is an entity attribute - hence the double .parent call
                        inputs.append(connection.parent.parent)
                        break
        except AttributeError:
            pass

        outputs = []
        try:
            for data in self.connections:
                entity_name = data.value.split(',')[0]
                matches = vmfFile.findKeyValue('targetname', entity_name)
                outputs.extend([m.parent for m in matches])
        except AttributeError:
            pass

        return inputs, outputs


class VmfFile(KeyValueFile):
    """contains more specific vmf related functionality"""

    # we don't need to do an initCache as thats done by the base class
    def __init__(self, filepath=None, chunk_class=VmfChunk, read_callback=None):
        KeyValueFile.__init__(self, filepath, parse_vmf_line, chunk_class, read_callback)

    def get_by_id(self, id):
        """given an id int this method will return the entity chunk with the given id - it ignores "side" chunks
        which occupy a different id space from world ents"""
        matches = self.find_key_value('id', str(id))
        if len(matches) == 1:
            return matches[0].parent
        else:
            non_side_matches = []
            for match in matches:
                # ok so now see if the match is a "side" object - if so we don't want it
                # side objects occupy a different id space than normal entities
                if match.parent.key == 'side':
                    continue
                else:
                    # there should only be one possible match to the id, so if its not a
                    # side entity, then return the first non-side match
                    return match.parent

        return None

    def create_entity(self, classname, key_value_pairs):
        new_entity = self.chunk_class('entity', [])
        key_value_pairs.insert(0, ("classname", classname))
        for key, value in key_value_pairs:
            new_entity.append(self.chunk_class(key, value, new_entity))

        self.data.append(new_entity)
        return new_entity

    def create_info_target(self, targetname, pos=(0, 0, 0), rot=(0, 0, 0)):
        return self.create_entity('info_target', [('id', self.get_unique_id()),
                                                  ('angles', ' '.join(map(str, rot))),
                                                  ('targetname', targetname),
                                                  ('origin', ' '.join(map(str, pos)))])

    def create_scripted_sequence(self, targetname, pos=(0, 0, 0), rot=(0, 0, 0)):
        attrs = [('id', self.get_unique_id()),
                 ('angles', ' '.join(map(str, rot))),
                 ('targetname', targetname),
                 ('origin', ' '.join(map(str, pos))),
                 ('m_bDisableNPCCollisions', '0'),
                 ('m_bIgnoreGravity', '0'),
                 ('m_bLoopActionSequence', '0'),
                 ('m_bSynchPostIdles', '0'),
                 ('m_flRadius', '0'),
                 ('m_flRepeat', '0'),
                 ('m_fMoveTo', '1'),
                 ('m_iszEntity', ''),
                 ('maxdxlevel', '0'),
                 ('mindxlevel', '0'),
                 ('onplayerdeath', '0'),
                 ('spawnflags', '0')]
        return self.create_entity('scripted_sequence', attrs)

    def get_unique_id(self):
        """returns an id number unique to the vmf file"""
        existing_ids = [int(id.value) for id in self.find_key('id')]
        existing_ids.sort()

        return existing_ids[-1] + 1

    def get_inputs_and_outputs(self, uid):
        entity = self.get_by_id(uid)
        return entity.getInputsAndOutputs(self)

# end
if __name__ == '__main__':
   a = VmfFile(r"G:\SteamLibrary\SteamApps\common\sourcesdk_content\tf\mapsrc\sdk_cp_gravelpit.vmf")
   ...
