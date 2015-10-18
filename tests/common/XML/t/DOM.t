#!/usr/bin/perl -w

use Test;

unless (eval "use XML::DOM; 1;") {
  print "1..0 # Skipped: XML::DOM not installed\n";
  exit;
}

plan tests => 35;

require XML::Generator::DOM;

my $x = new XML::Generator::DOM;
ok($x);

my $xml = $x->foo();
ok($xml->toString, '<foo/>');

$xml = $x->bar(42);
ok($xml->toString, '<bar>42</bar>');

$xml = $x->baz({'foo'=>3});
ok($xml->toString, '<baz foo="3"/>');

$xml = $x->bam({'bar'=>42},$x->foo(),"qux");
ok($xml->toString, '<bam bar="42"><foo/>qux</bam>');

$xml = $x->new(3);
ok($xml->toString, '<new>3</new>');

$xml = $x->foo(['baz']);
ok($xml->toString, '<baz:foo/>');

$xml = $x->foo(['baz'],{'bar'=>42},3);
ok($xml->toString, '<baz:foo baz:bar="42">3</baz:foo>');

$xml = $x->foo({'id' => 4}, 3, 5);
ok($xml->toString, '<foo id="4">35</foo>');

$xml = $x->foo({'id' => 4}, 0, 5);
ok($xml->toString, '<foo id="4">05</foo>');

$xml = $x->foo({'id' => 4}, 3, 0);
ok($xml->toString, '<foo id="4">30</foo>');

my $foo_bar = "foo-bar";
$xml = $x->$foo_bar(42);
ok($xml->toString, '<foo-bar>42</foo-bar>');

$x = new XML::Generator::DOM 'namespace' => ['A'];

$xml = $x->foo({'bar' => 42}, $x->bar(['B'], {'bar' => 54}));
ok($xml->toString, '<A:foo A:bar="42"><B:bar B:bar="54"/></A:foo>');

$xml = $x->xmldecl();
ok(UNIVERSAL::isa($xml, 'XML::DOM::XMLDecl'));

ok($xml->getVersion, '1.0');

ok($xml->getStandalone, 'yes');

$xml = $x->xmlcmnt("test");
ok(UNIVERSAL::isa($xml, 'XML::DOM::Comment'));

ok($xml->getData, 'test');

$x = new XML::Generator::DOM
			'version' => '1.1',
			'encoding' => 'iso-8859-2';
$xml = $x->xmldecl();
ok($xml->getVersion, '1.1');

ok($xml->getEncoding, 'iso-8859-2');

$xml = $x->xmlpi("target", 'option="value"');
ok(UNIVERSAL::isa($xml, 'XML::DOM::ProcessingInstruction'));

ok($xml->getTarget, 'target');

ok($xml->getData, 'option="value"');

eval {
  my $t = "42";
  $x->$t();
};
ok(UNIVERSAL::isa($@, 'XML::DOM::DOMException'));

$xml = $x->foo(['bar'], {'baz:foo' => 'qux', 'fob' => 'gux'});
ok($xml->toString eq '<bar:foo baz:foo="qux" bar:fob="gux"/>' ||
   $xml->toString eq '<bar:foo bar:fob="gux" baz:foo="qux"/>');

$x = new XML::Generator::DOM 'dtd' => [ 'foo', 'SYSTEM', '"http://foo.com/foo"' ];
$xml = $x->xmldecl();
ok($xml->getStandalone, 'no');

$xml = $x->xmlcdata("test");
ok(UNIVERSAL::isa($xml, 'XML::DOM::CDATASection'));

ok($xml->getData, 'test');

$x = new XML::Generator::DOM; 

$xml = $x->foo($x->xmlcdata("bar"), $x->xmlpi("baz", "bam"));
ok($xml->toString, '<foo><![CDATA[bar]]><?baz bam?></foo>');

$xml = $x->foo(42);
$xml = $x->xml($xml);
ok($xml->toString,
'<?xml version="1.0" standalone="yes"?>
<foo>42</foo>
');

eval {
  $xml = $x->bar($xml);
};
ok($@);
ok($@->getName, 'WRONG_DOCUMENT_ERR');

$xml = $x->foo();
$cmnt = $x->xmlcmnt("comment");
$pi = $x->xmlpi("foo", "bar");
$xml = $x->xml($cmnt, $xml, $pi);
ok($xml->toString, '<?xml version="1.0" standalone="yes"?>
<!--comment-->
<foo/>
<?foo bar?>
');

require XML::DOM;
$doc = XML::DOM::Parser->new->parse('<doc/>');
$x = XML::Generator::DOM->new( dom_document => $doc );
$doc->getFirstChild->appendChild($x->foo(42));
ok($doc->toString,
'<doc><foo>42</foo></doc>
');

eval {
  $xml = $x->xml($x->bar(12));
};
ok($@ =~ /method not allowed/);
