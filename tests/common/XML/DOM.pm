package XML::Generator::DOM;

=head1 NAME

XML::Generator::DOM - XML::Generator subclass for producing DOM trees instead of strings.

=head1 SYNOPSIS

	use XML::Generator::DOM;

	my $dg  = XML::Generator::DOM->new();
	my $doc = $dg->xml($dg->xmlcmnt("Test document."),
			   $dg->foo({'baz' => 'bam'}, 42));
	print $doc->toString;

yields:

	<?xml version="1.0" standalone="yes"?>
	<!--Test document-->
	<foo baz="bam">42</foo>

=head1 DESCRIPTION

XML::Generator::DOM subclasses XML::Generator in order to produce DOM
trees instead of strings (see L<XML::Generator> and L<XML::DOM>).  This
module is still experimental and its semantics might change.

Essentially, tag methods return XML::DOM::DocumentFragment objects,
constructed either from a DOM document passed into the constructor or
a default document that XML::Generator::DOM will automatically construct.

Calling the xml() method will return this automatically constructed
document and cause a fresh one to be constructed for future tag method
calls.  If you passed in your own document, you may not call the xml()
method.

Below, we just note the remaining differences in semantics between
XML::Generator methods and XML::Generator::DOM methods.

=cut

use strict;
use Carp;
use XML::Generator ();
use base 'XML::Generator';
use XML::DOM;

use vars qw( $AUTOLOAD $VERSION );

$VERSION = '0.2';

=head1 CONSTRUCTOR

These configuration options are accepted but have no effect on the
semantics of the returned object: escape, pretty, conformance and
empty.

=head1 TAG METHODS

Subsequently, tag method semantics are somewhat different for
this module compared to XML::Generator.  The primary difference is
that tag method return XML::DOM::DocumentFragment objects.  Namespace
and attribute processing remains the same, but remaining arguments to
tag methods must either be text or other XML::DOM::DocumentFragment
objects.  No escape processing, syntax checking, or output control is
done; this is all left up to XML::DOM.

=cut

sub new {
  my $class = shift;

  my $dom;
  for (my $i = 0; $i < $#_; $i+=2) {
    if ($_[$i] eq 'dom_document') {
      $dom = $_[$i+1];
      unless (UNIVERSAL::isa($dom, 'XML::DOM::Document')) {
	croak "argument to 'dom' option not an XML::DOM::Document object";
      }
      splice @_, $i, 2;
      last;
    }
  }

  if (ref $class) {
    $AUTOLOAD = 'new';
    return $class->AUTOLOAD(@_);
  }
  
  my $this = $class->SUPER::new(@_);

  $this->{'dom'} = $dom || XML::Generator::DOM::util::new_dom_root();
  return $this;
}

=head1 SPECIAL TAGS

All special tags are available by default with XML::Generator::DOM; you don't
need to use 'conformance' => 'strict'.

=head2 xmlpi(@args)

Arguments will simply be concatenated and passed as the data to
the XML::DOM::ProcessingInstruction object that is returned.

=cut

sub xmlpi {
  my $this = shift;
  my $root = $this->{dom};
  my $tgt = shift;
  return $root->createProcessingInstruction($tgt, join '', @_);
}

=head2 xmlcmnt

Escaping of '--' is done by XML::DOM::Comment, which replaces both
hyphens with '&#45;'.  An XML::DOM::Comment object is returned.

=cut

sub xmlcmnt {
  my $this = shift;
  my $root = $this->{dom};
  my $xml  = join '', @_;
  return $root->createComment($xml);
}

my $config = 'XML::Generator::util::config';

=head2 xmldecl

Returns an XML::DOM::XMLDecl object.  Respects 'version', 'encoding'
and 'dtd' settings in the object.

=cut

sub xmldecl {
  my $this = shift;
  my $root = $this->{dom};

  my $version  = $this->$config('version')  || '1.0';
  my $encoding = $this->$config('encoding') || undef;

  my $standalone = $this->xmldtd($this->$config('dtd'))
		   ? "no" : "yes";

  return $root->createXMLDecl($version, $encoding, $standalone)
}

=head2 xmldecl

Returns an XML::DOM::DocumentType object.

=cut

sub xmldtd {
  my($this, $dtd) = @_;
  my $root = $this->{dom};
  $dtd ||= $this->$config('dtd');
  return unless $dtd && ref($dtd) eq "ARRAY";

  return $root->createDocumentType(@{ $dtd });
}

=head2 xmlcdata

Returns an XML::DOM::CDATASection object.

=cut

sub xmlcdata {
  my $this = shift;
  my $data = join '', @_;
  my $root = $this->{dom};
  return $root->createCDATASection($data);
}

=head2 xml

As described above, xml() can only be used when dom_document was not
set in the object.  The automatically created document will have its XML
Declaration set and the arguments to xml() will be appended to it.  Then
a new DOM document is automatically generated and the old one is
returned.  This is the only way to get a DOM document from this module.

=cut

sub xml {
  my $this = shift;
  my $root = $this->{dom};

  if ($root != $XML::Generator::DOM::util::root) {
    croak "xml() method not allowed when dom_document option specified";
  }

  $this->{dom} = XML::Generator::DOM::util::new_dom_root();

  $root->setXMLDecl($this->xmldecl());

  $root->appendChild($_) for @_;
  return $root;
}

sub AUTOLOAD {
  my $this = shift;

  (my $tag = $AUTOLOAD) =~ s/.*:://;;

  my $root = $this->{'dom'};

  my($namespace, $attr, @args) = $this->XML::Generator::util::parse_args(@_);

  $namespace = $namespace->[1] ? $namespace->[1] . ':' : '';

  my $xml  = $root->createDocumentFragment();

  my $node = $xml->appendChild($root->createElement("$namespace$tag"));

  if ($attr) {
    while (my($k, $v) = each %$attr) {
      unless ($k =~ /^[^:]+:/) {
	$k = "$namespace$k";
      }
      $node->setAttribute($k, $v);
    }
  }

  for (@args) {
    if (UNIVERSAL::isa($_, 'XML::DOM::Node')) {
      $node->appendChild($_);
    } else {
      $node->appendChild($root->createTextNode($_));
    }
  }

  return $xml;
}

package XML::Generator::DOM::util;

use XML::DOM;
use vars qw($root $parser);

$parser = XML::DOM::Parser->new;

sub new_dom_root {
  $root = $parser->parse('<_/>');
  $root->removeChild($root->getFirstChild);

  return $root;
}

1;
