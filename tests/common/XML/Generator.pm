package XML::Generator;

use strict;
use Carp;
use vars qw/$VERSION $AUTOLOAD/;

$VERSION = '1.04';

=head1 NAME

XML::Generator - Perl extension for generating XML

=head1 SYNOPSIS

  use XML::Generator ':pretty';

  print foo(bar({ baz => 3 }, bam()),
	    bar([ 'qux' => 'http://qux.com/' ],
		  "Hey there, world"));

  # OR

  require XML::Generator;

  my $X = XML::Generator->new(':pretty');

  print $X->foo($X->bar({ baz => 3 }, $X->bam()),
		$X->bar([ 'qux' => 'http://qux.com/' ],
			  "Hey there, world"));
 
Either of the above yield:

   <foo xmlns:qux="http://qux.com/">
     <bar baz="3">
       <bam />
     </bar>
     <qux:bar>Hey there, world</qux:bar>
   </foo>

=head1 DESCRIPTION

In general, once you have an XML::Generator object, you then simply call
methods on that object named for each XML tag you wish to generate.

XML::Generator can also arrange for undefined subroutines in the caller's
package to generate the corresponding XML, by exporting an C<AUTOLOAD>
subroutine to your package.  Just supply an ':import' argument to
your C<use XML::Generator;> call.  If you already have an C<AUTOLOAD>
defined then XML::Generator can be configured to cooperate with it.
See L<"STACKABLE AUTOLOADs">.

Say you want to generate this XML:

   <person>
     <name>Bob</name>
     <age>34</age>
     <job>Accountant</job>
   </person>

Here's a snippet of code that does the job, complete with pretty printing:

   use XML::Generator;
   my $gen = XML::Generator->new(':pretty');
   print $gen->person(
            $gen->name("Bob"),
            $gen->age(34),
            $gen->job("Accountant")
         );

The only problem with this is if you want to use a tag name that
Perl's lexer won't understand as a method name, such as "shoe-size".
Fortunately, since you can store the name of a method in a variable,
there's a simple work-around:

   my $shoe_size = "shoe-size";
   $xml = $gen->$shoe_size("12 1/2");

Which correctly generates:

   <shoe-size>12 1/2</shoe-size>

You can use a hash ref as the first parameter if the tag should include
atributes.  Normally this means that the order of the attributes will be
unpredictable, but if you have the L<Tie::IxHash> module, you can use it
to get the order you want, like this:

  use Tie::IxHash;
  tie my %attr, 'Tie::IxHash';

  %attr = (name => 'Bob', 
	   age  => 34,
	   job  => 'Accountant',
    'shoe-size' => '12 1/2');

  print $gen->person(\%attr);

This produces

  <person name="Bob" age="34" job="Accountant" shoe-size="12 1/2" />

An array ref can also be supplied as the first argument to indicate
a namespace for the element and the attributes.

If there is one element in the array, it is considered the URI of
the default namespace, and the tag will have an xmlns="URI" attribute
added automatically.  If there are two elements, the first should be
the tag prefix to use for the namespace and the second element should
be the URI.  In this case, the prefix will be used for the tag and an
xmlns:PREFIX attribute will be automatically added.  Prior to version
0.99, this prefix was also automatically added to each attribute name.
Now, the default behavior is to leave the attributes alone (although you
may always explicitly add a prefix to an attribute name).  If the prior
behavior is desired, use the constructor option C<qualified_attributes>.

If you specify more than two elements, then each pair should correspond
to a tag prefix and the corresponding URL.  An xmlns:PREFIX attribute
will be added for each pair, and the prefix from the first such pair
will be used as the tag's namespace.  If you wish to specify a default
namespace, use '#default' for the prefix.  If the default namespace is
first, then the tag will use the default namespace itself.

If you want to specify a namespace as well as attributes, you can make
the second argument a hash ref.  If you do it the other way around,
the array ref will simply get stringified and included as part of the
content of the tag.

Here's an example to show how the attribute and namespace parameters work:

   $xml = $gen->account(
	    $gen->open(['transaction'], 2000),
	    $gen->deposit(['transaction'], { date => '1999.04.03'}, 1500)
          );

This generates:

   <account>
     <open xmlns="transaction">2000</open>
     <deposit xmlns="transaction" date="1999.04.03">1500</deposit>
   </account>

Because default namespaces inherit, XML::Generator takes care to output
the xmlns="URI" attribute as few times as strictly necessary.  For example,

   $xml = $gen->account(
	    $gen->open(['transaction'], 2000),
	    $gen->deposit(['transaction'], { date => '1999.04.03'},
	      $gen->amount(['transaction'], 1500)
	    )
          );

This generates:

   <account>
     <open xmlns="transaction">2000</open>
     <deposit xmlns="transaction" date="1999.04.03">
       <amount>1500</amount>
     </deposit>
   </account>

Notice how C<xmlns="transaction"> was left out of the C<<amount>> tag.

Here is an example that uses the two-argument form of the namespace:

    $xml = $gen->widget(['wru' => 'http://www.widgets-r-us.com/xml/'],
                        {'id'  => 123}, $gen->contents());

    <wru:widget xmlns:wru="http://www.widgets-r-us.com/xml/" id="123">
      <contents />
    </wru:widget>

Here is an example that uses multiple namespaces.  It generates the
first example from the RDF primer (L<http://www.w3.org/TR/rdf-primer/>).

    my $contactNS = [contact => "http://www.w3.org/2000/10/swap/pim/contact#"];
    $xml = $gen->xml(
	     $gen->RDF([ rdf     => "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
		         @$contactNS ],
		$gen->Person($contactNS, { 'rdf:about' => "http://www.w3.org/People/EM/contact#me" },
		  $gen->fullName($contactNS, 'Eric Miller'),
		  $gen->mailbox($contactNS, {'rdf:resource' => "mailto:em@w3.org"}),
		  $gen->personalTitle($contactNS, 'Dr.'))));

    <?xml version="1.0" standalone="yes"?>
    <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
	     xmlns:contact="http://www.w3.org/2000/10/swap/pim/contact#">
      <contact:Person rdf:about="http://www.w3.org/People/EM/contact#me">
	<contact:fullName>Eric Miller</contact:fullName>
	<contact:mailbox rdf:resource="mailto:em@w3.org" />
	<contact:personalTitle>Dr.</contact:personalTitle> 
      </Person>
    </rdf:RDF>

=head1 CONSTRUCTOR

XML::Generator-E<gt>new(':option', ...);

XML::Generator-E<gt>new(option => 'value', ...);

(Both styles may be combined)

The following options are available:

=head2 :std, :standard

Equivalent to 

	escape      => 'always',
	conformance => 'strict',

=head2 :strict

Equivalent to

	conformance => 'strict',

=head2 :pretty[=N]

Equivalent to

	escape      => 'always',
	conformance => 'strict',
	pretty      => N         # N defaults to 2

=head2 namespace

This value of this option must be an array reference containing one or
two values.  If the array contains one value, it should be a URI and will
be the value of an 'xmlns' attribute in the top-level tag.  If there are
two or more elements, the first of each pair should be the namespace
tag prefix and the second the URI of the namespace.  This will enable
behavior similar to the namespace behavior in previous versions; the tag
prefix will be applied to each tag.  In addition, an xmlns:NAME="URI"
attribute will be added to the top-level tag.  Prior to version 0.99,
the tag prefix was also automatically added to each attribute name,
unless overridden with an explicit prefix.  Now, the attribute names are
left alone, but if the prior behavior is desired, use the constructor
option C<qualified_attributes>.

The value of this option is used as the global default namespace.
For example,

    my $html = XML::Generator->new(
                 pretty    => 2,
                 namespace => [HTML => "http://www.w3.org/TR/REC-html40"]);
    print $html->html(
	    $html->body(
	      $html->font({ face => 'Arial' },
			  "Hello, there")));

would yield

    <HTML:html xmlns:HTML="http://www.w3.org/TR/REC-html40">
      <HTML:body>
        <HTML:font face="Arial">Hello, there</HTML:font>
      </HTML:body>
    </HTML:html>

Here is the same example except without all the prefixes:

    my $html = XML::Generator->new(
		 pretty    => 2,
                 namespace => ["http://www.w3.org/TR/REC-html40"]);
    print $html->html(
	    $html->body(
	      $html->font({ 'face' => 'Arial' },
			    "Hello, there")));

would yield

   <html xmlns="http://www.w3.org/TR/REC-html40">
     <body>
        <font face="Arial">Hello, there</font>
     </body>
   </html>

=head2 qualifiedAttributes, qualified_attributes

Set this to a true value to emulate the attribute prefixing behavior of
XML::Generator prior to version 0.99.  Here is an example:

    my $foo = XML::Generator->new(
                namespace => [foo => "http://foo.com/"],
		qualifiedAttributes => 1);
    print $foo->bar({baz => 3});

yields

    <foo:bar xmlns:foo="http://foo.com/" foo:baz="3" />

=head2 escape

The contents and the values of each attribute have any illegal XML
characters escaped if this option is supplied.  If the value is 'always',
then &, < and > (and " within attribute values) will be converted into
the corresponding XML entity, although & will not be converted if it looks
like it could be part of a valid entity (but see below).  If the value is
'unescaped', then the escaping will be turned off character-by- character
if the character in question is preceded by a backslash, or for the
entire string if it is supplied as a scalar reference.  So, for example,

	use XML::Generator escape => 'always';

	one('<');      # <one>&lt;</one>
	two('\&');     # <two>\&amp;</two>
	three(\'>');   # <three>&gt;</three> (scalar refs always allowed)
	four('&lt;');  # <four>&lt;</four> (looks like an entity)
	five('&#34;'); # <five>&#34;</five> (looks like an entity)

but

	use XML::Generator escape => 'unescaped';

	one('<');     # <one>&lt;</one>
	two('\&');    # <two>&</two>
	three(\'>');  # <three>></three> (aiee!)
	four('&lt;'); # <four>&amp;lt;</four> (no special case for entities)

By default, high-bit data will be passed through unmodified, so that
UTF-8 data can be generated with pre-Unicode perls.  If you know that
your data is ASCII, use the value 'high-bit' for the escape option
and bytes with the high bit set will be turned into numeric entities.
You can combine this functionality with the other escape options by
comma-separating the values:

  my $a = XML::Generator->new(escape => 'always,high-bit');
  print $a->foo("<\242>");

yields

  <foo>&lt;&#162;&gt;</foo>

Because XML::Generator always uses double quotes ("") around attribute
values, it does not escape single quotes.  If you want single quotes
inside attribute values to be escaped, use the value 'apos' along with
'always' or 'unescaped' for the escape option.  For example:

    my $gen = XML::Generator->new(escape => 'always,apos');
    print $gen->foo({'bar' => "It's all good"});

    <foo bar="It&apos;s all good" />

If you actually want & to be converted to &amp; even if it looks like it
could be part of a valid entity, use the value 'even-entities' along with
'always'.  Supplying 'even-entities' to the 'unescaped' option is meaningless
as entities are already escaped with that option.

=head2 pretty

To have nice pretty printing of the output XML (great for config files
that you might also want to edit by hand), supply an integer for the
number of spaces per level of indenting, eg.

   my $gen = XML::Generator->new(pretty => 2);
   print $gen->foo($gen->bar('baz'),
                   $gen->qux({ tricky => 'no'}, 'quux'));

would yield

   <foo>
     <bar>baz</bar>
     <qux tricky="no">quux</qux>
   </foo>

You may also supply a non-numeric string as the argument to 'pretty', in
which case the indents will consist of repetitions of that string.  So if
you want tabbed indents, you would use:

     my $gen = XML::Generator->new(pretty => "\t");

Pretty printing does not apply to CDATA sections or Processing Instructions.

=head2 conformance

If the value of this option is 'strict', a number of syntactic
checks are performed to ensure that generated XML conforms to the
formal XML specification.  In addition, since entity names beginning
with 'xml' are reserved by the W3C, inclusion of this option enables
several special tag names: xmlpi, xmlcmnt, xmldecl, xmldtd, xmlcdata,
and xml to allow generation of processing instructions, comments, XML
declarations, DTD's, character data sections and "final" XML documents,
respectively.

Invalid characters (http://www.w3.org/TR/xml11/#charsets) will be filtered
out.  To disable this behavior, supply the 'filter_invalid_chars' option with
the value 0.

See L<"XML CONFORMANCE"> and L<"SPECIAL TAGS"> for more information.

=head2 filterInvalidChars, filter_invalid_chars

Set this to a 1 to enable filtering of invalid characters, or to 0 to disable
the filtering.  See http://www.w3.org/TR/xml11/#charsets for the set of valid
characters.

=head2 allowedXMLTags, allowed_xml_tags

If you have specified 'conformance' => 'strict' but need to use tags
that start with 'xml', you can supply a reference to an array containing
those tags and they will be accepted without error.  It is not an error
to supply this option if 'conformance' => 'strict' is not supplied,
but it will have no effect.

=head2 empty

There are 5 possible values for this option:

   self    -  create empty tags as <tag />  (default)
   compact -  create empty tags as <tag/>
   close   -  close empty tags as <tag></tag>
   ignore  -  don't do anything (non-compliant!)
   args    -  use count of arguments to decide between <x /> and <x></x>

Many web browsers like the 'self' form, but any one of the forms besides
'ignore' is acceptable under the XML standard.

'ignore' is intended for subclasses that deal with HTML and other
SGML subsets which allow atomic tags.  It is an error to specify both
'conformance' => 'strict' and 'empty' => 'ignore'.

'args' will produce <x /> if there are no arguments at all, or if there
is just a single undef argument, and <x></x> otherwise.

=head2 version

Sets the default XML version for use in XML declarations.
See L<"xmldecl"> below.

=head2 encoding

Sets the default encoding for use in XML declarations.

=head2 dtd

Specify the dtd.  The value should be an array reference with three
values; the type, the name and the uri.

=head1 IMPORT ARGUMENTS

use XML::Generator ':option';

use XML::Generator option => 'value';

(Both styles may be combined)

=head2 :import

Cause C<use XML::Generator;> to export an C<AUTOLOAD> to your package that
makes undefined subroutines generate XML tags corresponding to their name.
Note that if you already have an C<AUTOLOAD> defined, it will be overwritten.

=head2 :stacked

Implies :import, but if there is already an C<AUTOLOAD> defined, the
overriding C<AUTOLOAD> will still give it a chance to run.  See L<"STACKED
AUTOLOADs">.

=head2 ANYTHING ELSE

If you supply any other options, :import is implied and the XML::Generator
object that is created to generate tags will be constructed with those options.

=cut

package XML::Generator;

use strict;
require Carp;

# If no value is provided for these options, they will be set to ''

my @optionsToInit = qw(
  allowed_xml_tags
  conformance
  dtd
  escape
  namespace
  pretty
  version
  empty
  qualified_attributes
  filter_invalid_chars
);

my %tag_factory;

sub import {
  my $type = shift;

  # check for attempt to use tag 'import'
  if (ref $type && defined $tag_factory{$type}) {
    unshift @_, $type, 'import';
    goto &{ $tag_factory{$type} };
  }

  my $pkg = caller;

  no strict 'refs'; # Let's get serious

  # should we import an AUTOLOAD?
  no warnings 'once';

  if (@_) {
    my $STACKED;

    # are we supposed to call their AUTOLOAD first?
    if (grep /^:stacked$/, @_) {
      $STACKED = \&{"${pkg}::AUTOLOAD"};
    }

    my $this = $type->new(@_);

    no warnings 'redefine'; # No, I mean SERIOUS

    *{"${pkg}::AUTOLOAD"} =
      sub {
	if ($STACKED) {
	  ${"${pkg}::AUTOLOAD"} = our $AUTOLOAD;
	  my @ret = $STACKED->(@_);
	  return wantarray ? @ret : $ret[0] if @ret;
	}

	# The tag is whatever our sub name is.
	my($tag) = our $AUTOLOAD =~ /.*::(.*)/;

	# Special-case for xml... tags
	if ($tag =~ /^xml/ && $this->{'conformance'} eq 'strict') {
	  if (my $func = $this->can($tag)) {
	    unshift @_, $this;
	    goto &$func;
	  }
	}

	unshift @_, $this, $tag;

	goto &{ $tag_factory{$this} };
      };

    # convenience feature for stacked autoloads; give them
    # an import() that aliases AUTOLOAD.
    if ($STACKED && ! defined *{"${pkg}::import"}{CODE}) {
      *{"${pkg}::import"} =
        sub {
	  my $p = caller;
	  *{"${p}::AUTOLOAD"} = \&{"${pkg}::AUTOLOAD"};
	};
    }
  }

  return;
}

# The constructor method

sub new {
  my $class = shift;

  # If we already have a ref in $class, this means that the
  # person wants to generate a <new> tag!
  return $class->XML::Generator::util::tag('new', @_) if ref $class;

  my %options =
    map {
      /^:(std|standard)  $/x  ? ( escape      => 'always',
			          conformance => 'strict' )
    : /^:strict          $/x  ? ( conformance => 'strict' )
    : /^:pretty(?:=(.+))?$/x  ? ( escape      => 'always',
			          conformance => 'strict',
			          pretty      => ( defined $1 ? $1 : 2 ) )
    : /^:(import |
         stacked        )$/x  ? ( do { Carp::carp("Useless use of $_")
				         unless (caller(1))[3] =~ /::import/;
				  () } )
    : /^allowedXMLTags$/      ? 'allowed_xml_tags'
    : /^qualifiedAttributes$/ ? 'qualified_attributes'
    : /^filterInvalidChars$/  ? 'filter_invalid_chars'
    : $_
    } @_;

  # We used to only accept certain options, but unfortunately this
  # means that subclasses can't extend the list. As such, we now 
  # just make sure our default options are defined.
  for (@optionsToInit) {
    if (not defined $options{$_}) {
       $options{$_} = '';
    }
  }

  if ($options{'dtd'}) {
    $options{'dtdtree'} = $class->XML::Generator::util::parse_dtd($options{'dtd'});
  }

  if ($options{'conformance'} eq 'strict' &&
      $options{'empty'}       eq 'ignore') {
    Carp::croak "option 'empty' => 'ignore' not allowed while 'conformance' => 'strict'";
  }

  if ($options{'escape'}) {
    my $e = $options{'escape'};
    $options{'escape'} = 0;
    while ($e =~ /([-\w]+),?/g) {
      if ($1 eq 'always') {
	$options{'escape'} |= XML::Generator::util::ESCAPE_ALWAYS()
			   |  XML::Generator::util::ESCAPE_GT();
      } elsif ($1 eq 'high-bit') {
	$options{'escape'} |= XML::Generator::util::ESCAPE_HIGH_BIT();
      } elsif ($1 eq 'apos') {
	$options{'escape'} |= XML::Generator::util::ESCAPE_APOS();
      } elsif ($1 eq 'even-entities') {
	$options{'escape'} |= XML::Generator::util::ESCAPE_EVEN_ENTITIES();
      } elsif ($1) {
	if ($1 ne 'unescaped') {
	  Carp::carp "option 'escape' => '$1' deprecated; use 'escape' => 'unescaped'";
	}
	$options{'escape'} |= XML::Generator::util::ESCAPE_TRUE()
			   |  XML::Generator::util::ESCAPE_GT();
      }
    }
  } else {
    $options{'escape'} = 0;
  }

  if (ref $options{'namespace'} eq 'ARRAY') {
    if (@{ $options{'namespace'} } > 2 && (@{ $options{'namespace'} } % 2) != 0) {
      Carp::croak "odd number of arguments for namespace";
    }
  } elsif ($options{'namespace'}) {
    Carp::croak "namespace must be an array reference";
  }

  if ($options{'conformance'} eq 'strict' &&
      $options{'filter_invalid_chars'} eq '') {
    $options{'filter_invalid_chars'} = 1;
  }

  my $this = bless \%options, $class;
  $tag_factory{$this} = XML::Generator::util::c_tag($this);
  return $this;
}

# We use AUTOLOAD as a front-end to TAG so that we can
# create tags by name at will.

sub AUTOLOAD {
  my $this = shift;

  # The tag is whatever our sub name is, or 'AUTOLOAD'
  my ($tag) = defined our $AUTOLOAD ? $AUTOLOAD =~ /.*::(.*)/ : 'AUTOLOAD';

  undef $AUTOLOAD; # this ensures that future attempts to use tag 'AUTOLOAD' work.

  unshift @_, $this, $tag;

  goto &{ $tag_factory{$this} };
}

# I wish there were a way to allow people to use tag 'DESTROY!'
# hmm, maybe xmlDESTROY?
sub DESTROY { delete $tag_factory{$_[0]} }

=head1 XML CONFORMANCE

When the 'conformance' => 'strict' option is supplied, a number of
syntactic checks are enabled.  All entity and attribute names are
checked to conform to the XML specification, which states that they must
begin with either an alphabetic character or an underscore and may then
consist of any number of alphanumerics, underscores, periods or hyphens.
Alphabetic and alphanumeric are interpreted according to the current
locale if 'use locale' is in effect and according to the Unicode standard
for Perl versions >= 5.6.  Furthermore, entity or attribute names are not
allowed to begin with 'xml' (in any case), although a number of special
tags beginning with 'xml' are allowed (see L<"SPECIAL TAGS">). Note
that you can also supply an explicit list of allowed tags with the
'allowed_xml_tags' option.

Also, the filter_invalid_chars option is automatically set to 1 unless it
is explicitly set to 0.

=head1 SPECIAL TAGS

The following special tags are available when running under strict
conformance (otherwise they don't act special):

=head2 xmlpi

Processing instruction; first argument is target, remaining arguments
are attribute, value pairs.  Attribute names are syntax checked, values
are escaped.

=cut

# We handle a few special tags, but only if the conformance
# is 'strict'. If not, we just fall back to XML::Generator::util::tag.

sub xmlpi {
  my $this = shift;

  return $this->XML::Generator::util::tag('xmlpi', @_)
		unless $this->{conformance} eq 'strict';

  my $xml;
  my $tgt  = shift;

  $this->XML::Generator::util::ck_syntax($tgt);

  $xml = "<?$tgt";
  if (@_) {
     my %atts = @_;
     while (my($k, $v) = each %atts) {
       $this->XML::Generator::util::ck_syntax($k);
       XML::Generator::util::escape($v,
                XML::Generator::util::ESCAPE_FILTER_INVALID_CHARS() |
                XML::Generator::util::ESCAPE_ATTR() |
                $this->{'escape'});
       $xml .= qq{ $k="$v"};
     }
  }
  $xml .= "?>";

  return XML::Generator::pi->new([$xml]);
}

=head2 xmlcmnt

Comment.  Arguments are concatenated and placed inside <!-- ... -->
comment delimiters.  Any occurences of '--' in the concatenated arguments
are converted to '&#45;&#45;'

=cut

sub xmlcmnt {
  my $this = shift;

  return $this->XML::Generator::util::tag('xmlcmnt', @_)
		unless $this->{conformance} eq 'strict';

  my $xml = join '', @_;

  # double dashes are illegal; change them to '&#45;&#45;'
  $xml =~ s/--/&#45;&#45;/g;
  XML::Generator::util::filter($xml);
  $xml = "<!-- $xml -->";

  return XML::Generator::comment->new([$xml]);
}

=head2 xmldecl(@args)

Declaration.  This can be used to specify the version, encoding, and
other XML-related declarations (i.e., anything inside the <?xml?> tag).
@args can be used to control what is output, as keyword-value pairs.

By default, the version is set to the value specified in the constructor,
or to 1.0 if it was not specified.  This can be overridden by providing a
'version' key in @args.  If you do not want the version at all, explicitly
provide undef as the value in @args.

By default, the encoding is set to the value specified in the constructor;
if no value was specified, the encoding will be left out altogether.
Provide an 'encoding' key in @args to override this.

If a dtd was set in the constructor, the standalone attribute of the
declaration will be set to 'no' and the doctype declaration will be
appended to the XML declartion, otherwise the standalone attribute will
be set to 'yes'.  This can be overridden by providing a 'standalone'
key in @args.  If you do not want the standalone attribute to show up,
explicitly provide undef as the value.

=cut

sub xmldecl {
  my($this, @args) = @_;

  return $this->XML::Generator::util::tag('xmldecl', @_)
		unless $this->{conformance} eq 'strict';

  my $version  = $this->{'version'} || '1.0';

  # there's no explicit support for encodings yet, but at the
  # least we can know to put it in the declaration
  my $encoding = $this->{'encoding'};

  # similarly, although we don't do anything with DTDs yet, we
  # recognize a 'dtd' => [ ... ] option to the constructor, and
  # use it to create a <!DOCTYPE ...> and to indicate that this
  # document can't stand alone.
  my $doctype = $this->xmldtd($this->{dtd});
  my $standalone = $doctype ? "no" : "yes";

  for (my $i = 0; $i < $#args; $i += 2) {
         if ($args[$i] eq 'version'   ) {
      $version    = $args[$i + 1];
    } elsif ($args[$i] eq 'encoding'  ) {
      $encoding   = $args[$i + 1];
    } elsif ($args[$i] eq 'standalone') {
      $standalone = $args[$i + 1];
    } else {
      Carp::croak("Unrecognized argument '$args[$i]'");
    }
  }

  $version    =    qq{ version="$version"}    if defined    $version;
  $encoding   =   qq{ encoding="$encoding"}   if defined   $encoding;
  $standalone = qq{ standalone="$standalone"} if defined $standalone;

  $encoding   ||= '';
  $version    ||= '';
  $standalone ||= '';

  my $xml = "<?xml$version$encoding$standalone?>";
  $xml .= "\n$doctype" if $doctype;

  $xml = "$xml\n";

  return $xml;
}

=head2 xmldtd

DTD <!DOCTYPE> tag creation. The format of this method is different from
others. Since DTD's are global and cannot contain namespace information,
the first argument should be a reference to an array; the elements are
concatenated together to form the DTD:

   print $xml->xmldtd([ 'html', 'PUBLIC', $xhtml_w3c, $xhtml_dtd ])

This would produce the following declaration:

   <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
        "DTD/xhtml1-transitional.dtd">

Assuming that $xhtml_w3c and $xhtml_dtd had the correct values.

Note that you can also specify a DTD on creation using the new() method's
dtd option.

=cut

sub xmldtd {
  my $this = shift;
  my $dtd = shift || return undef;

  # return the appropriate <!DOCTYPE> thingy
  $dtd ? return(qq{<!DOCTYPE } . (join ' ', @{$dtd}) . q{>})
       : return('');
}

=head2 xmlcdata

Character data section; arguments are concatenated and placed inside
<![CDATA[ ... ]]> character data section delimiters.  Any occurences of
']]>' in the concatenated arguments are converted to ']]&gt;'.

=cut

sub xmlcdata {
  my $this = shift;

  $this->XML::Generator::util::tag('xmlcdata', @_)
		unless $this->{conformance} eq 'strict';

  my $xml = join '', @_;

  # ]]> is not allowed; change it to ]]&gt;
  $xml =~ s/]]>/]]&gt;/g;
  XML::Generator::util::filter($xml);
  $xml = "<![CDATA[$xml]]>";

  return XML::Generator::cdata->new([$xml]);
}

=head2 xml

"Final" XML document.  Must be called with one and exactly one
XML::Generator-produced XML document.  Any combination of
XML::Generator-produced XML comments or processing instructions may
also be supplied as arguments.  Prepends an XML declaration, and
re-blesses the argument into a "final" class that can't be embedded.

=cut

sub xml {
  my $this = shift;

  return $this->XML::Generator::util::tag('xml', @_)
		unless $this->{conformance} eq 'strict';

  unless (@_) {
    Carp::croak "usage: object->xml( (COMMENT | PI)* XML (COMMENT | PI)* )";
  }

  my $got_root = 0;
  foreach my $arg (@_) {
    next if UNIVERSAL::isa($arg, 'XML::Generator::comment') ||
	    UNIVERSAL::isa($arg, 'XML::Generator::pi');
    if (UNIVERSAL::isa($arg, 'XML::Generator::overload')) {
      if ($got_root) {
	Carp::croak "arguments to xml() can contain only one XML document";
      }
      $got_root = 1;
    } else {
      Carp::croak "arguments to xml() must be comments, processing instructions or XML documents";
    }
  }

  return XML::Generator::final->new([$this->xmldecl(), @_]);
}

=head1 CREATING A SUBCLASS

For a simpler way to implement subclass-like behavior, see L<"STACKABLE
AUTOLOADs">.

At times, you may find it desireable to subclass XML::Generator. For
example, you might want to provide a more application-specific interface
to the XML generation routines provided. Perhaps you have a custom
database application and would really like to say:

   my $dbxml = new XML::Generator::MyDatabaseApp;
   print $dbxml->xml($dbxml->custom_tag_handler(@data));

Here, custom_tag_handler() may be a method that builds a recursive XML
structure based on the contents of @data. In fact, it may even be named
for a tag you want generated, such as authors(), whose behavior changes
based on the contents (perhaps creating recursive definitions in the
case of multiple elements).

Creating a subclass of XML::Generator is actually relatively
straightforward, there are just three things you have to remember:

   1. All of the useful utilities are in XML::Generator::util.

   2. To construct a tag you simply have to call SUPER::tagname,
      where "tagname" is the name of your tag.

   3. You must fully-qualify the methods in XML::Generator::util.

So, let's assume that we want to provide a custom HTML table() method:

   package XML::Generator::CustomHTML;
   use base 'XML::Generator';

   sub table {
       my $self = shift;
       
       # parse our args to get namespace and attribute info
       my($namespace, $attr, @content) =
          $self->XML::Generator::util::parse_args(@_)

       # check for strict conformance
       if ( $self->XML::Generator::util::config('conformance') eq 'strict' ) {
          # ... special checks ...
       }

       # ... special formatting magic happens ...

       # construct our custom tags
       return $self->SUPER::table($attr, $self->tr($self->td(@content)));
   }

That's pretty much all there is to it. We have to explicitly call
SUPER::table() since we're inside the class's table() method. The others
can simply be called directly, assuming that we don't have a tr() in the
current package.

If you want to explicitly create a specific tag by name, or just want a
faster approach than AUTOLOAD provides, you can use the tag() method
directly. So, we could replace that last line above with:

       # construct our custom tags 
       return $self->XML::Generator::util::tag('table', $attr, ...);

Here, we must explicitly call tag() with the tag name itself as its first
argument so it knows what to generate. These are the methods that you might
find useful:

=over 4

=item XML::Generator::util::parse_args()

This parses the argument list and returns the namespace (arrayref), attributes
(hashref), and remaining content (array), in that order.

=item XML::Generator::util::tag()

This does the work of generating the appropriate tag. The first argument must
be the name of the tag to generate.

=item XML::Generator::util::config()

This retrieves options as set via the new() method.

=item XML::Generator::util::escape()

This escapes any illegal XML characters.

=back

Remember that all of these methods must be fully-qualified with the
XML::Generator::util package name. This is because AUTOLOAD is used by 
the main XML::Generator package to create tags. Simply calling parse_args()
will result in a set of XML tags called <parse_args>.

Finally, remember that since you are subclassing XML::Generator, you do
not need to provide your own new() method. The one from XML::Generator
is designed to allow you to properly subclass it.

=head1 STACKABLE AUTOLOADs

As a simpler alternative to traditional subclassing, the C<AUTOLOAD>
that C<use XML::Generator;> exports can be configured to work with a
pre-defined C<AUTOLOAD> with the ':stacked' option.  Simply ensure that
your C<AUTOLOAD> is defined before C<use XML::Generator ':stacked';>
executes.  The C<AUTOLOAD> will get a chance to run first; the subroutine
name will be in your C<$AUTOLOAD> as normal.  Return an empty list to let
the default XML::Generator C<AUTOLOAD> run or any other value to abort it.
This value will be returned as the result of the original method call.

If there is no C<import> defined, XML::Generator will create one.
All that this C<import> does is export AUTOLOAD, but that lets your
package be used as if it were a subclass of XML::Generator.

An example will help:

	package MyGenerator;

	my %entities = ( copy => '&copy;',
			 nbsp => '&nbsp;', ... );

	sub AUTOLOAD {
	  my($tag) = our $AUTOLOAD =~ /.*::(.*)/;

	  return $entities{$tag} if defined $entities{$tag};
	  return;
	}

	use XML::Generator qw(:pretty :stacked);

This lets someone do:

	use MyGenerator;

	print html(head(title("My Title", copy())));

Producing:

	<html>
	  <head>
	    <title>My Title&copy;</title>
	  </head>
	</html>

=cut

package XML::Generator::util;

# The ::util package space actually has all the utilities
# that do all the work. It must be separate from the
# main XML::Generator package space since named subs will
# interfere with the workings of AUTOLOAD otherwise.

use strict;
use Carp;

use constant ESCAPE_TRUE     => 1;
use constant ESCAPE_ALWAYS   => 1<<1;
use constant ESCAPE_HIGH_BIT => 1<<2;
use constant ESCAPE_APOS     => 1<<3;
use constant ESCAPE_ATTR     => 1<<4;
use constant ESCAPE_GT       => 1<<5;
use constant ESCAPE_EVEN_ENTITIES => 1<<6;
use constant ESCAPE_FILTER_INVALID_CHARS => 1<<7;

sub parse_args {
  # this parses the args and returns a namespace and attr
  # if either were specified, with the remainer of the
  # arguments (the content of the tag) in @args. call as:
  #
  #   ($namespace, $attr, @args) = parse_args(@args);
 
  my($this, @args) = @_;

  my($namespace);
  my($attr) = ('');

  # check for supplied namespace
  if (ref $args[0] eq 'ARRAY') {
    $namespace = [ map { defined $_ && $_ eq '#default' ? undef : $_ } @{shift @args} ];
    if (@$namespace > 2 && (@$namespace % 2) != 0) {
      croak "odd number of arguments for namespace";
    }
  }

  # get globally-set namespace (from new)
  unless ($namespace) {
    $namespace = [ map { defined $_ && $_ eq '#default' ? undef : $_ } @{ $this->{'namespace'} || [] } ];
  }

  if (@$namespace == 1) { unshift @$namespace, undef }

  # check for supplied attributes
  if (ref $args[0] eq 'HASH') {
    $attr = shift @args;
    if ($this->{conformance} eq 'strict') {
      $this->XML::Generator::util::ck_syntax($_)
	for map split(/:/), keys %$attr;
    }
  }

  return ($namespace, $attr, @args);
}

# This routine is what handles all the automatic tag creation.
# We maintain it as a separate method so that subclasses can
# override individual tags and then call SUPER::tag() to create
# the tag automatically. This is not possible if only AUTOLOAD
# is used, since there is no way to then pass in the name of
# the tag.

sub tag {
  my $sub  = XML::Generator::util::c_tag($_[0]);
  goto &{ $sub } if $sub;
}
 
# Generate a closure that encapsulates all the behavior to generate a tag
sub c_tag {
  my $arg = shift;

  my $strict = $arg->{'conformance'} eq 'strict';
  my $escape = $arg->{'escape'};
  my $empty  = $arg->{'empty'};
  my $indent = $arg->{'pretty'} =~ /^[^0-9]/
             ? $arg->{'pretty'}
             : $arg->{'pretty'}
               ? " " x $arg->{'pretty'}
               : "";
  if ($arg->{'filter_invalid_chars'}) {
    $escape |= ESCAPE_FILTER_INVALID_CHARS;
  }

  my $blessClass = $indent ? 'XML::Generator::pretty' : 'XML::Generator::overload';

  return sub {
    my $this = shift;
    my $tag = shift || return undef;   # catch for bad usage

    # parse our argument list to check for hashref/arrayref properties
    my($namespace, $attr, @args) = $this->XML::Generator::util::parse_args(@_);

    $this->XML::Generator::util::ck_syntax($tag) if $strict;

    # check for attempt to embed "final" document
    for (@args) {
      if (UNIVERSAL::isa($_, 'XML::Generator::final')) {
	croak("cannot embed XML document");
      }
    }

    # Deal with escaping if required
   if ($escape) {
      if ($attr) {
	foreach my $key (keys %{$attr}) {
	  next unless defined($attr->{$key});
	  XML::Generator::util::escape($attr->{$key}, ESCAPE_ATTR() | $escape);
	}
      }
      for (@args) {
	next unless defined($_);

	# perform escaping, except on sub-documents or simple scalar refs
	if (ref $_ eq "SCALAR") {
	  # un-ref it
	  $_ = $$_;
	} elsif (! UNIVERSAL::isa($_, 'XML::Generator::overload') ) {
	  XML::Generator::util::escape($_, $escape);
	}
      }
    } else {
      # un-ref simple scalar refs
      for (@args) {
	$_ = $$_ if ref $_ eq "SCALAR";
      }
    }

    my $prefix = '';
    $prefix = $namespace->[0] . ":" if $namespace && defined $namespace->[0];
    my $xml = "<$prefix$tag";

    if ($attr) {
      while (my($k, $v) = each %$attr) {
	next unless defined $k and defined $v;
	if ($strict) {
	  # allow supplied namespace in attribute names
	  if ($k =~ s/^([^:]+)://) {
	    $this->XML::Generator::util::ck_syntax($k);
	    $k = "$1:$k";
	  } elsif ($prefix && $this->{'qualified_attributes'}) {
	    $this->XML::Generator::util::ck_syntax($k);
	    $k = "$prefix$k";
	  } else {
	    $this->XML::Generator::util::ck_syntax($k);
	  }
	} elsif ($this->{'qualified_attributes'}) {
	  if ($k !~ /^[^:]+:/) {
	    $k = "$prefix$k";
	  }
	}
	$xml .= qq{ $k="$v"};
      }
    }

    my @xml;

    if (@args || $empty eq 'close') {
      if ($empty eq 'args' && @args == 1 && ! defined $args[0]) {
	@xml = ($xml .= ' />');
      } else {
	$xml .= '>';
	if ($indent) {
	  my $prettyend = '';

	  foreach my $arg (@args) {
	    next unless defined $arg;
	    if ( UNIVERSAL::isa($arg, 'XML::Generator::cdata'   ) ) {
	      my $copy = $xml;
	      push @xml, $copy, $arg;
	      $xml = '';
	    } else {
	      if ( UNIVERSAL::isa($arg, 'XML::Generator::overload') &&
	           ! UNIVERSAL::isa($arg, 'XML::Generator::pi') ) {
		$xml .= "\n$indent";
		$prettyend = "\n";
		XML::Generator::util::_fixupNS($namespace, $arg) if ref $arg->[0];

		my @cdata;
		for my $i (0..$#$arg) {
		  if (UNIVERSAL::isa($arg->[$i], 'XML::Generator::cdata')) {
		    push @cdata, $arg->[$i];
		    $arg->[$i] = "\001";
		  }
		}

		$arg =~ s/\n/\n$indent/gs;

		if (@cdata) {
		  my @pieces = split "\001", $arg;

		  my $copy = $xml;
		  push @xml, $copy;
		  $xml = '';
		  $arg = '';

		  for my $i (0..$#pieces) {
		    if (defined $cdata[$i]) {
		      push @xml, $pieces[$i], $cdata[$i];
		    } else {
		      push @xml, $pieces[$i];
		    }
		  }
		}
	      }
	      $xml .= "$arg";
	    }
	  }
	  $xml .= $prettyend;
	  push @xml, ($xml, "</$prefix$tag>");
	} else {
	  @xml = $xml;
	  foreach my $arg (grep defined, @args) {
	    if ( UNIVERSAL::isa($arg, 'XML::Generator::overload') &&
	    (! ( UNIVERSAL::isa($arg, 'XML::Generator::cdata'   ) ||
		 UNIVERSAL::isa($arg, 'XML::Generator::pi'      )))) {
	      XML::Generator::util::_fixupNS($namespace, $arg) if ref $arg->[0];
	    }
	    push @xml, $arg;
          }
          push @xml, "</$prefix$tag>";
	}
      }
    } elsif ($empty eq 'ignore') {
      @xml = ($xml .= '>');
    } elsif ($empty eq 'compact') {
      @xml = ($xml .= '/>');
    } else {
      @xml = ($xml .= ' />');
    }

    unshift @xml, $namespace if $namespace;

    return $blessClass->new(\@xml);
  };
}

sub _fixupNS {
  # remove namespaces
  # if prefix
  #    if prefix and uri match one we have, remove them from child
  #    if prefix does not match one we have, remove it and uri
  #      from child and add them to us
  # no prefix
  #    if we have an explicit default namespace and the child has the
  #      same one, remove it from the child
  #    if we have an explicit default namespace and the child has a
  #      different one, leave it alone
  #    if we have an explicit default namespace and the child has none,
  #      add an empty default namespace to child
  my($namespace, $o) = @_;
  my @n = @{$o->[0]};
  my $sawDefault = 0;
  for (my $i = 0; $i < $#n; $i+=2) {
    if (defined $n[$i]) { # namespace w/ prefix
      my $flag = 0;
      for (my $j = 0; $j < $#$namespace; $j+=2) {
	next unless defined $namespace->[$j];
	if ($namespace->[$j] eq $n[$i]) {
	  $flag = 1;
	  if ($namespace->[$j+1] ne $n[$i+1]) {
	    $flag = 2;
	  }
	  last;
	}
      }
      if (!$flag) {
	push @$namespace, splice @n, $i, 2;
	$i-=2;
      } elsif ($flag == 1) {
	splice @n, $i, 2;
	$i-=2;
      }
    } elsif (defined $n[$i+1]) { # default namespace
      $sawDefault = 1;
      for (my $j = 0; $j < $#$namespace; $j+=2) {
	next if defined $namespace->[$j];
        if ($namespace->[$j+1] eq $n[$i+1]) {
	  splice @n, $i, 2;
	  $i-=2;
	}
      }
    }
  }

  # check to see if we need to add explicit default namespace of "" to child
  if (! @{ $o->[0] } &&
      ! $sawDefault &&
      grep { defined $namespace->[$_ * 2 + 1] &&
           ! defined $namespace->[$_ * 2    ] } 0..($#$namespace/2)) {
    push @n, undef, "";
  }

  if (@n) {
    $o->[0] = [@n];
  } else {
    splice @$o, 0, 1;
  }
}

# Fetch and store config values (those set via new())
# This is only here for subclasses

sub config {
  my $this = shift;
  my $key = shift || return undef;
  @_ ? $this->{$key} = $_[0]
     : $this->{$key};
}

# Collect all escaping into one place
sub escape {
  # $_[0] is the argument, $_[1] are the flags
  return unless defined $_[0];

  my $f = $_[1];
  if ($f & ESCAPE_ALWAYS) {
    if ($f & ESCAPE_EVEN_ENTITIES) {
      $_[0] =~ s/&/&amp;/g;
    } else {
      $_[0] =~ s/&(?!(?:#[0-9]+|#x[0-9a-fA-F]+|\w+);)/&amp;/g;
    }

    $_[0] =~ s/</&lt;/g;
    $_[0] =~ s/>/&gt;/g   if $f & ESCAPE_GT;
    $_[0] =~ s/"/&quot;/g if $f & ESCAPE_ATTR;
    $_[0] =~ s/'/&apos;/g if $f & ESCAPE_ATTR && $f & ESCAPE_APOS;
  } else {
    $_[0] =~ s/([^\\]|^)&/$1&amp;/g;
    $_[0] =~ s/\\&/&/g;
    $_[0] =~ s/([^\\]|^)</$1&lt;/g;
    $_[0] =~ s/\\</</g;
    if ($f & ESCAPE_GT) {
      $_[0] =~ s/([^\\]|^)>/$1&gt;/g;
      $_[0] =~ s/\\>/>/g;
    }
    if ($f & ESCAPE_ATTR) {
      $_[0] =~ s/([^\\]|^)"/$1&quot;/g;
      $_[0] =~ s/\\"/"/g;
      if ($f & ESCAPE_APOS) {
      $_[0] =~ s/([^\\]|^)'/$1&apos;/g;
      $_[0] =~ s/\\'/'/g;
      }
    }
  } 
  if ($f & ESCAPE_HIGH_BIT) {
    $_[0] =~ s/([\200-\377])/'&#'.ord($1).';'/ge;
  }
  if ($f & ESCAPE_FILTER_INVALID_CHARS) {
    filter($_[0]);
  }
}

sub filter { $_[0] =~ tr/\x00\x01\x02\x03\x04\x05\x06\x07\x08\x0B\x0C\x0E\x0F\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1A\x1B\x1C\x1D\x1E\x1F\x7F\x80\x81\x82\x83\x84\x86\x87\x88\x89\x8A\x8B\x8C\x8D\x8E\x8F\x90\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9A\x9B\x9C\x9D\x9E\x9F//d }

# verify syntax of supplied name; croak if it's not valid.
# rules: 1. name must begin with a letter or an underscore
#        2. name may contain any number of letters, numbers, hyphens,
#           periods or underscores
#        3. name cannot begin with "xml" in any case
sub ck_syntax {
  my($this, $name) = @_;
  # use \w and \d so that everything works under "use locale" and 
  # "use utf8"
  if ($name =~ /^\w[\w\-\.]*$/) {
    if ($name =~ /^\d/) {
      croak "name [$name] may not begin with a number";
    }
  } else {
    croak "name [$name] contains illegal character(s)";
  }
  if ($name =~ /^xml/i) {
    if (!$this->{'allowed_xml_tags'} || ! grep { $_ eq $name } @{ $this->{'allowed_xml_tags'} }) {
      croak "names beginning with 'xml' are reserved by the W3C";
    }
  }
}

my %DTDs;
my $DTD;

sub parse_dtd {
  my $this = shift;
  my($dtd) = @_;

  my($root, $type, $name, $uri);

  croak "DTD must be supplied as an array ref" unless (ref $dtd eq 'ARRAY');
  croak "DTD must have at least 3 elements" unless (@{$dtd} >= 3);

  ($root, $type) = @{$dtd}[0,1];
  if ($type eq 'PUBLIC') {
    ($name, $uri) = @{$dtd}[2,3];
  } elsif ($type eq 'SYSTEM') {
    $uri = $dtd->[2];
  } else {
    croak "unknown dtd type [$type]";
  }
  return $DTDs{$uri} if $DTDs{$uri};

  # parse DTD into $DTD (not implemented yet)
  my $dtd_text = get_dtd($uri);

  return $DTDs{$uri} = $DTD;
}

sub get_dtd {
  my($uri) = @_;
  return;
}

# This package is needed so that embedded tags are correctly
# interpreted as such and handled properly. Otherwise, you'd
# get "<outer>&lt;inner /&gt;</outer>"

package XML::Generator::overload;

use overload '""'   => sub { $_[0]->stringify },
             '0+'   => sub { $_[0]->stringify },
             'bool' => sub { $_[0]->stringify },
             'eq'   => sub { (ref $_[0] ? $_[0]->stringify : $_[0]) eq
			     (ref $_[1] ? $_[1]->stringify : $_[1])};

sub new {
  my($class, $xml) = @_;
  return bless $xml, $class;
}

sub stringify {
  return $_[0] unless UNIVERSAL::isa($_[0], 'XML::Generator::overload');
  if (ref($_[0]->[0])) { # namespace
    my $n = shift @{$_[0]};
    for (my $i = ($#$n - 1); $i >= 0; $i-=2) {
      my($prefix, $uri) = @$n[$i,$i+1];
      XML::Generator::util::escape($uri, XML::Generator::util::ESCAPE_ATTR  |
                                         XML::Generator::util::ESCAPE_ALWAYS|
                                         XML::Generator::util::ESCAPE_GT);
      if (defined $prefix) {
        $_[0]->[0] =~ s/^([^ \/>]+)/$1 xmlns:$prefix="$uri"/;
      } else {
        $uri ||= '';
        $_[0]->[0] =~ s/^([^ \/>]+)/$1 xmlns="$uri"/;
      }
    }
  }

  join $, || "", @{$_[0]}
}

sub DESTROY { }

package XML::Generator::pretty;

use base 'XML::Generator::overload';

sub stringify {
  my $this = shift;
  my $string = $this->SUPER::stringify();

  $string =~ s{^((\s*<(?:\w+:)?\w[-.\w]* )[^ "]+"[^"]+")( .{40,})}
	      {  my($a,$b,$c) = ($1, $2, $3);
		$c =~ s{ ((?:\w+:)?\w+="[^\"]+")}{"\n" . (' 'x(length $b)) . $1}ge;
		"$a$c" }gem;

  return $string;
}

package XML::Generator::final;

use base 'XML::Generator::overload';

package XML::Generator::comment;

use base 'XML::Generator::overload';

package XML::Generator::pi;

use base 'XML::Generator::overload';

package XML::Generator::cdata;

use base 'XML::Generator::overload';

1;
__END__

=head1 AUTHORS

=over 4

=item Benjamin Holzman <bholzman@earthlink.net>

Original author and maintainer

=item Bron Gondwana <perlcode@brong.net>

First modular version

=item Nathan Wiger <nate@nateware.com>

Modular rewrite to enable subclassing

=back

=head1 SEE ALSO

=over 4

=item The XML::Writer module

http://search.cpan.org/search?mode=module&query=XML::Writer

=back

=cut
