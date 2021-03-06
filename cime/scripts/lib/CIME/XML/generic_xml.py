"""
Common interface to XML files, this is an abstract class and is expected to
be used by other XML interface modules and not directly.
"""
from CIME.XML.standard_module_setup import *
from distutils.spawn import find_executable
from xml.dom import minidom
import getpass

logger = logging.getLogger(__name__)

class GenericXML(object):

    def __init__(self, infile=None, schema=None):
        """
        Initialize an object
        """
        logger.debug("Initializing {}".format(infile))
        self.tree = None

        if infile == None:
            # if file is not defined just return
            self.filename = None
            return

        if os.path.isfile(infile) and os.access(infile, os.R_OK):
            # If file is defined and exists, read it
            self.filename = infile
            self.read(infile, schema)
        else:
            # if file does not exist create a root xml element
            # and set it's id to file

            logger.debug("File {} does not exists.".format(infile))
            expect("$" not in infile,"File path not fully resolved {}".format(infile))

            self.filename = infile
            root = ET.Element("xml")
            self.root = ET.SubElement(root, "file")
            self.root.set("id", os.path.basename(infile))
            self.root.set("version", "2.0")
            self.tree = ET.ElementTree(root)

    def read(self, infile, schema=None):
        """
        Read and parse an xml file into the object
        """
        logger.debug("read: " + infile)
        if self.tree:
            self.root.append(ET.parse(infile).getroot())
        else:
            self.tree = ET.parse(infile)
            self.root = self.tree.getroot()

        if schema is not None and self.get_version() > 1.0:
            self.validate_xml_file(infile, schema)

        logger.debug("File version is {}".format(str(self.get_version())))

    def get_version(self):
        version = self.root.get("version")
        version = 1.0 if version is None else float(version)
        return version

    def write(self, outfile=None):
        """
        Write an xml file from data in self
        """
        if outfile is None:
            outfile = self.filename

        logger.debug("write: " + outfile)

        xmlstr = self.get_raw_record()

        # xmllint provides a better format option for the output file
        xmllint = find_executable("xmllint")
        if xmllint is not None:
            run_cmd_no_fail("{} --format --output {} -".format(xmllint, outfile), input_str=xmlstr)
        else:
            doc = minidom.parseString(xmlstr)
            with open(outfile,'w') as xmlout:
                doc.writexml(xmlout,addindent='  ')

    def get_node(self, nodename, attributes=None, root=None, xpath=None):
        """
        Get an xml element matching nodename with optional attributes.

        Error unless exactly one match.
        """

        nodes = self.get_nodes(nodename, attributes=attributes, root=root, xpath=xpath)

        expect(len(nodes) == 1, "Incorrect number of matches, {:d}, for nodename '{}' and attrs '{}' in file '{}'".format(len(nodes), nodename, attributes, self.filename))
        return nodes[0]

    def get_optional_node(self, nodename, attributes=None, root=None, xpath=None):
        """
        Get an xml element matching nodename with optional attributes.

        Return None if no match.
        """
        nodes = self.get_nodes(nodename, attributes=attributes, root=root, xpath=xpath)

        expect(len(nodes) <= 1, "Multiple matches for nodename '{}' and attrs '{}' in file '{}'".format(nodename, attributes, self.filename))
        return nodes[0] if nodes else None

    def get_nodes(self, nodename, attributes=None, root=None, xpath=None):

        logger.debug("(get_nodes) Input values: {}, {}, {}, {}, {}".format(self.__class__.__name__, nodename, attributes, root, xpath))

        if root is None:
            root = self.root
        nodes = []

        expect(attributes is None or xpath is None,
               " Arguments attributes and xpath are exclusive")
        if xpath is None:
            xpath = ".//"+nodename

        if attributes:
            # xml.etree has limited support for xpath and does not allow more than
            # one attribute in an xpath query so we query seperately for each attribute
            # and create a result with the intersection of those lists

            for key, value in attributes.iteritems():
                if value is not None:
                    expect(isinstance(value, basestring),
                           " Bad value passed for key {}".format(key))
                    xpath = ".//{}[@{}=\'{}\']".format(nodename, key, value)
                    logger.debug("xpath is {}".format(xpath))

                    try:
                        newnodes = root.findall(xpath)
                    except Exception as e:
                        expect(False, "Bad xpath search term '{}', error: {}".format(xpath, e))

                    if not nodes:
                        nodes = newnodes
                    else:
                        for node in nodes[:]:
                            if node not in newnodes:
                                nodes.remove(node)
                    if not nodes:
                        return []

        else:
            logger.debug("xpath: {}".format(xpath))
            nodes = root.findall(xpath)

        logger.debug("Returning {} nodes ({})".format(len(nodes), nodes))

        return nodes

    def add_child(self, node, root=None):
        """
        Add element node to self at root
        """
        if root is None:
            root = self.root
        self.root.append(node)

    def get_value(self, item, attribute=None, resolved=True, subgroup=None): # pylint: disable=unused-argument
        """
        get_value is expected to be defined by the derived classes, if you get here
        the value was not found in the class.
        """
        logger.debug("Get Value for " + item)
        return None

    def get_values(self, vid, attribute=None, resolved=True, subgroup=None):# pylint: disable=unused-argument
        logger.debug("Get Values for " + vid)
        return []

    def set_value(self, vid, value, subgroup=None, ignore_type=True): # pylint: disable=unused-argument
        """
        ignore_type is not used in this flavor
        """
        valnodes = self.get_nodes(vid)
        if valnodes:
            for node in valnodes:
                node.text = value

    def get_resolved_value(self, raw_value):
        """
        A value in the xml file may contain references to other xml
        variables or to environment variables. These are refered to in
        the perl style with $name and $ENV{name}.

        >>> obj = GenericXML()
        >>> os.environ["FOO"] = "BAR"
        >>> os.environ["BAZ"] = "BARF"
        >>> obj.get_resolved_value("one $ENV{FOO} two $ENV{BAZ} three")
        'one BAR two BARF three'
        >>> obj.get_resolved_value("2 + 3 - 1")
        '4'
        >>> obj.get_resolved_value("0001-01-01")
        '0001-01-01'
        >>> obj.get_resolved_value("$SHELL{echo hi}")
        'hi'
        """
        logger.debug("raw_value {}".format(raw_value))
        reference_re = re.compile(r'\${?(\w+)}?')
        env_ref_re   = re.compile(r'\$ENV\{(\w+)\}')
        shell_ref_re = re.compile(r'\$SHELL\{([^}]+)\}')
        math_re = re.compile(r'\s[+-/*]\s')
        item_data = raw_value

        if item_data is None:
            return None

        if type(item_data) is not str:
            return item_data

        for m in env_ref_re.finditer(item_data):
            logger.debug("look for {} in env".format(item_data))
            env_var = m.groups()[0]
            env_var_exists = env_var in os.environ
            expect(env_var_exists, "Undefined env var '{}'".format(env_var))
            item_data = item_data.replace(m.group(), os.environ[env_var])

        for s in shell_ref_re.finditer(item_data):
            logger.debug("execute {} in shell".format(item_data))
            shell_cmd = s.groups()[0]
            item_data = item_data.replace(s.group(), run_cmd_no_fail(shell_cmd))

        for m in reference_re.finditer(item_data):
            var = m.groups()[0]
            logger.debug("find: {}".format(var))
            ref = self.get_value(var)
            if ref is not None:
                logger.debug("resolve: " + str(ref))
                item_data = item_data.replace(m.group(), self.get_resolved_value(str(ref)))
            elif var == "CIMEROOT":
                cimeroot = get_cime_root()
                item_data = item_data.replace(m.group(), cimeroot)
            elif var == "SRCROOT":
                srcroot = os.path.join(get_cime_root(),"..")
                item_data = item_data.replace(m.group(), srcroot)
            elif var == "USER":
                item_data = item_data.replace(m.group(), getpass.getuser())

        if math_re.search(item_data):
            try:
                tmp = eval(item_data)
            except:
                tmp = item_data
            item_data = str(tmp)

        return item_data

    def add_sub_node(self, node, subnode_name, subnode_text):
        expect(node is not None," Bad value passed")
        subnode = ET.Element(subnode_name)
        subnode.text = subnode_text
        node.append(subnode)
        return node

    def validate_xml_file(self, filename, schema):
        """
        validate an XML file against a provided schema file using pylint
        """
        expect(os.path.isfile(filename),"xml file not found {}".format(filename))
        expect(os.path.isfile(schema),"schema file not found {}".format(schema))
        xmllint = find_executable("xmllint")
        if xmllint is not None:
            logger.debug("Checking file {} against schema {}".format(filename, schema))
            run_cmd_no_fail("{} --noout --schema {} {}".format(xmllint, schema, filename))
        else:
            logger.warn("xmllint not found, could not validate file {}".format(filename))

    def get_element_text(self, element_name, attributes=None, root=None, xpath=None):
        element_node = self.get_optional_node(element_name, attributes, root, xpath)
        if element_node is not None:
            return element_node.text
        return None

    def set_element_text(self, element_name, new_text, attributes=None, root=None, xpath=None):
        element_node = self.get_optional_node(element_name, attributes, root, xpath)
        if element_node is not None:
            element_node.text = new_text
            return new_text
        return None

    def get_raw_record(self, root=None):
        logger.debug("writing file {}".format(self.filename))
        if root is None:
            root = self.root
        try:
            xmlstr = ET.tostring(root)
        except ET.ParseError as e:
            ET.dump(root)
            expect(False, "Could not write file {}, xml formatting error '{}'".format(self.filename, e))
        return xmlstr

    def get_id(self):
        xmlid = self.root.get("id")
        if xmlid is not None:
            return xmlid
        return self.root.tag
