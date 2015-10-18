#!/usr/bin/perl -w

use Test;

BEGIN { $| = 1; plan tests => 100; }

use XML::Generator ();
ok(1);

my $x = XML::Generator->new();
ok($x);

my $xml = $x->foo();
ok($xml, '<foo />');

$xml = $x->bar(42);
ok($xml, '<bar>42</bar>');

$xml = $x->baz({'foo'=>3});
ok($xml, '<baz foo="3" />');

$xml = $x->bam({'bar'=>42},$x->foo(),"qux");
ok($xml, '<bam bar="42"><foo />qux</bam>');

eval { require Tie::IxHash; };
if ($@) {
  skip('Tie::IxHash not installed', 1);
} else {
  tie %h, 'Tie::IxHash';
  @h{'a'..'z'} = 1..26;
  $xml = $x->foo(\%h);
  ok($xml, '<foo ' . join(' ', map qq($_="$h{$_}"), keys %h) . ' />');
}


$xml = $x->new(3);
ok($xml, '<new>3</new>');

$xml = $x->import(3);
ok($xml, '<import>3</import>');

$xml = $x->foo(['baz']);
ok($xml, '<foo xmlns="baz" />');

$xml = $x->foo(['baz','bam']);
ok($xml, '<baz:foo xmlns:baz="bam" />');

$xml = $x->foo(['baz'],{'bar'=>42},3);
ok($xml, '<foo xmlns="baz" bar="42">3</foo>');

$xml = $x->foo(['baz','bam'],{'bar'=>42},3);
ok($xml, '<baz:foo xmlns:baz="bam" bar="42">3</baz:foo>');

$xml = $x->foo({'id' => 4}, 3, 5);
ok($xml, '<foo id="4">35</foo>');

$xml = $x->foo({'id' => 4}, 0, 5);
ok($xml, '<foo id="4">05</foo>');

$xml = $x->foo({'id' => 4}, 3, 0);
ok($xml, '<foo id="4">30</foo>');

my $foo_bar = "foo-bar";
$xml = $x->$foo_bar(42);
ok($xml, '<foo-bar>42</foo-bar>');

$x = new XML::Generator 'escape' => 'always';

$xml = $x->foo({'bar' => '4"4'}, '<&>"\<', \"<>");
ok($xml, '<foo bar="4&quot;4">&lt;&amp;&gt;"\&lt;<></foo>');

$x = new XML::Generator 'escape' => 'unescaped';

$xml = $x->foo({'bar' => '4\"4'}, '<&>"\<', \"&& 6 < 5");
ok($xml, '<foo bar="4"4">&lt;&amp;&gt;"<&& 6 < 5</foo>');

$x = new XML::Generator 'namespace' => ['A'];

$xml = $x->foo({'bar' => 42}, $x->bar(['B'], {'bar' => 54}));
ok($xml, '<foo xmlns="A" bar="42"><bar xmlns="B" bar="54" /></foo>');

$x = new XML::Generator 'conformance' => 'strict';
$xml = $x->xmldecl();
ok($xml, qq(<?xml version="1.0" standalone="yes"?>\n));

$xml = $x->xmlcmnt("test");
ok($xml, '<!-- test -->');

$x = new XML::Generator 'conformance' => 'strict',
			'version' => '1.1',
			'encoding' => 'iso-8859-2';
$xml = $x->xmldecl();
ok($xml, qq(<?xml version="1.1" encoding="iso-8859-2" standalone="yes"?>\n));

$xml = $x->xmldecl(version => undef, encoding => undef, standalone => undef);
ok($xml, qq(<?xml?>\n));

$xml = $x->xmldecl(version => '1.0', encoding => 'utf8', standalone => 'no');
ok($xml, qq(<?xml version="1.0" encoding="utf8" standalone="no"?>\n));

$xml = $x->xmlpi("target", "option" => "value");
ok($xml, '<?target option="value"?>');

eval {
  $x->xmlfoo();
};
ok($@, qr{names beginning with 'xml' are reserved by the W3C});

eval {
  $x->foo({xmlfoo => 4});
};
ok($@, qr{names beginning with 'xml' are reserved by the W3C});

eval {
  my $t = "42";
  $x->$t();
};
ok($@, qr{name \[42] may not begin with a number});

eval {
  $x->q({42=>'the answer'});
};
ok($@, qr{name \[42] may not begin with a number});

eval {
  my $t = "g:";
  $x->$t();
};
ok($@, qr{name \[g:] contains illegal character\(s\)});

$xml = $x->foo(['bar'], {'baz:foo' => 'qux', 'fob' => 'gux'});
ok($xml eq '<foo xmlns="bar" baz:foo="qux" fob="gux" />' ||
   $xml eq '<foo xmlns="bar" fob="gux" baz:foo="qux" />', 1, $xml);

$xml = $x->foo(['bar' => 'bam'], {'baz:foo' => 'qux', 'fob' => 'gux'});
ok($xml eq '<bar:foo xmlns:bar="bam" baz:foo="qux" fob="gux" />' ||
   $xml eq '<bar:foo xmlns:bar="bam" fob="gux" baz:foo="qux" />', 1, $xml);

$x = new XML::Generator;
$xml = $x->xml();
ok($xml, '<xml />');

$x = new XML::Generator 'conformance' => 'strict',
			'dtd' => [ 'foo', 'SYSTEM', '"http://foo.com/foo"' ];
$xml = $x->xmldecl();
ok($xml,
'<?xml version="1.0" standalone="no"?>
<!DOCTYPE foo SYSTEM "http://foo.com/foo">
');

$xml = $x->xmlcdata("test");
ok($xml, '<![CDATA[test]]>');

$x = new XML::Generator 'pretty' => 2, 'conformance' => 'strict';
$xml = $x->foo($x->bar());
ok($xml,
'<foo>
  <bar />
</foo>');

$xml = $x->foo($x->xmlcdata("bar"), $x->xmlpi("baz"));
ok($xml, '<foo><![CDATA[bar]]><?baz?></foo>');

# test that cdata is not intended when pretty printing is on

$xml = $x->foo($x->bam($x->xmlcdata("bar\nbar")));
ok($xml, '<foo>
  <bam><![CDATA[bar
bar]]></bam>
</foo>');

$x = new XML::Generator 'conformance' => 'strict';
$xml = $x->foo(42);
$xml = $x->xml($xml);
ok($xml,
'<?xml version="1.0" standalone="yes"?>
<foo>42</foo>');

eval {
  $x->xml();
};
ok($@ =~ /usage/, 1);

eval {
  $x->xml(3);
};
ok($@ =~ /arguments to xml/, 1);

eval {
  $xml = $x->bar($xml);
};
ok($@ =~ /cannot embed/, 1);

$x = new XML::Generator 'pretty' => 2;
$xml = $x->foo($x->bar($x->baz()));
ok($xml,
'<foo>
  <bar>
    <baz />
  </bar>
</foo>');

$xml = $x->foo("\n<bar />");
ok($xml,
'<foo>
<bar /></foo>');

$x = new XML::Generator 'empty' => 'close';
$xml = $x->foo();
ok($xml, '<foo></foo>');

$x = new XML::Generator 'empty' => 'ignore';
$xml = $x->foo();
ok($xml, '<foo>');

eval {
  $x = new XML::Generator 'empty' => 'ignore', 'conformance' => 'strict';
};
ok($@ =~ /not allowed/, 1);

$x = new XML::Generator 'conformance' => 'strict';
$xml = $x->foo();
$cmnt = $x->xmlcmnt("comment");
$pi = $x->xmlpi("foo");
$xml = $x->xml($cmnt, $xml, $pi);
ok($xml, '<?xml version="1.0" standalone="yes"?>
<!-- comment --><foo /><?foo?>');

$x = new XML::Generator 'empty' => 'compact';
$xml = $x->foo();
ok($xml, '<foo/>');

$x = new XML::Generator 'empty' => 'args';
$xml = $x->foo(1);
ok($xml, '<foo>1</foo>');

$xml = $x->foo('');
ok($xml, '<foo></foo>');

$xml = $x->foo();
ok($xml, '<foo />');

$xml = $x->foo(undef);
ok($xml, '<foo />');

$x = XML::Generator->new(escape => 'always,high-bit');
$xml = $x->foo("<\242>");
ok($xml, '<foo>&lt;&#162;&gt;</foo>');

# check :options
$x = XML::Generator->new(':standard');
$xml = $x->foo('<', $x->xmlcmnt('c'));
ok($xml, '<foo>&lt;<!-- c --></foo>');

$x = XML::Generator->new(':pretty');
$xml = $x->foo('<', $x->bar($x->xmlcmnt('c')));
ok($xml, '<foo>&lt;
  <bar>
    <!-- c -->
  </bar>
</foo>');

$x = XML::Generator->new(':strict', escape => 'high-bit');
$xml = $x->foo("\\<\242", $x->xmlpi('g'));
ok($xml, '<foo><&#162;<?g?></foo>');

{ my $w; local $SIG{__WARN__} = sub { $w .= $_[0] };
  $x = XML::Generator->new(':import');
  ok($w =~ /Useless use of/, 1); $w = '';
}

# test AUTOLOAD
package Test1;

use XML::Generator ':import';

::ok(foo(), '<foo />');

package Test2;

use XML::Generator ':pretty';

::ok(foo(bar()), '<foo>
  <bar />
</foo>');

package Test3;

sub AUTOLOAD {
  return "foo" if our $AUTOLOAD =~ /bar/;
  return;
}

use XML::Generator;

::ok(barnyard(), 'foo');
::ok(foo(), undef);

package Test6;

sub AUTOLOAD {
  return "foo" if our $AUTOLOAD =~ /bar/;
  return;
}

use XML::Generator qw(:import);

::ok(barnyard(), '<barnyard />');
::ok(foo(), '<foo />');

package Test7;

sub AUTOLOAD {
  return "foo" if our $AUTOLOAD =~ /bar/;
  return;
}

use XML::Generator qw(:stacked);

::ok(barnyard(), 'foo');
::ok(foo(), '<foo />');
::ok(foo(barnyard()), '<foo>foo</foo>');

# misc

package main;

$x = XML::Generator->new(':strict', allowed_xml_tags => ['xmlfoo']);

$xml = $x->xmlfoo('biznatch');
ok($xml, '<xmlfoo>biznatch</xmlfoo>');

$xml = $x->xmlcmnt('--');
ok($xml, '<!-- &#45;&#45; -->');

$A = XML::Generator->new(namespace => ['A']);
$B = XML::Generator->new(namespace => ['B' => 'bee']);
$xml = $A->foo($B->bar($A->baz()));
ok($xml, '<foo xmlns="A" xmlns:B="bee"><B:bar><baz xmlns="A" /></B:bar></foo>');

$xml = $A->foo($A->bar($B->baz()));
ok($xml, '<foo xmlns="A" xmlns:B="bee"><bar><B:baz /></bar></foo>');

$xml = $A->foo($B->bar($B->baz()));
ok($xml, '<foo xmlns="A" xmlns:B="bee"><B:bar><B:baz /></B:bar></foo>');

$C = XML::Generator->new(namespace => [undef]);
$xml = $A->foo($C->bar($B->baz()));
ok($xml, '<foo xmlns="A" xmlns:B="bee"><bar xmlns=""><B:baz /></bar></foo>');

$D = XML::Generator->new();
$xml = $D->foo(['A'],$D->bar([undef],$D->baz(['B'=>'bee'])));
ok($xml, '<foo xmlns="A" xmlns:B="bee"><bar xmlns=""><B:baz /></bar></foo>');

$E = XML::Generator->new();
$xml = $E->foo(['A'],$E->bar([undef],$E->baz(['B'=>'bee'], $E->bum(['A']))));
ok($xml, '<foo xmlns="A" xmlns:B="bee"><bar xmlns=""><B:baz><bum xmlns="A" /></B:baz></bar></foo>');

package MyGenerator;

sub AUTOLOAD {
  my($tag) = our $AUTOLOAD =~ /.*::(.*)/;

  return '&copy;' if $tag eq 'copy';
  return;
}

use XML::Generator qw(:pretty :stacked);

package Test8;

MyGenerator->import();

$xml = html(title("My Title",copy()));
::ok($xml,
'<html>
  <title>My Title&copy;</title>
</html>');

package TestDoc1_1;

  use XML::Generator ':pretty';

  $prt =   foo(bar({ baz => 3 }, bam()),
            bar([ 'qux' => 'http://qux.com/' ],
                  "Hey there, world"));

::ok($prt,
'<foo xmlns:qux="http://qux.com/">
  <bar baz="3">
    <bam />
  </bar>
  <qux:bar>Hey there, world</qux:bar>
</foo>');

package TestDoc1_2;

  use XML::Generator ();

  my $X = XML::Generator->new(':pretty');

  $prt = $X->foo($X->bar({ baz => 3 }, $X->bam()),
                 $X->bar([ 'qux' => 'http://qux.com/' ],
                           "Hey there, world"));

::ok($prt,
'<foo xmlns:qux="http://qux.com/">
  <bar baz="3">
    <bam />
  </bar>
  <qux:bar>Hey there, world</qux:bar>
</foo>');

package TestDoc2;

   use XML::Generator;
   my $gen = XML::Generator->new(':pretty');
   $prt = $gen->person(
             $gen->name("Bob"),
             $gen->age(34),
             $gen->job("Accountant")
          );

::ok($prt,
'<person>
  <name>Bob</name>
  <age>34</age>
  <job>Accountant</job>
</person>');

   my $shoe_size = "shoe-size";
   $xml = $gen->$shoe_size("12 1/2");

::ok($xml, '<shoe-size>12 1/2</shoe-size>');

  $xml = $gen->account(
            $gen->open(['transaction'], 2000),
            $gen->deposit(['transaction'], { date => '1999.04.03'}, 1500)
          );

::ok($xml,
'<account>
  <open xmlns="transaction">2000</open>
  <deposit xmlns="transaction" date="1999.04.03">1500</deposit>
</account>');

  $xml = $gen->account(
            $gen->open(['transaction'], 2000),
            $gen->deposit(['transaction'], { date => '1999.04.03'},
	      $gen->amount(['transaction'], 1500)
	    )
          );

::ok($xml,
'<account>
  <open xmlns="transaction">2000</open>
  <deposit xmlns="transaction" date="1999.04.03">
    <amount>1500</amount>
  </deposit>
</account>');

  $xml = $gen->widget(['wru' => 'http://www.widgets-r-us.com/xml/'],
                      {'id'  => 123}, $gen->contents());

::ok($xml,
'<wru:widget xmlns:wru="http://www.widgets-r-us.com/xml/" id="123">
  <contents />
</wru:widget>');

package TestDoc3;

    my $html = XML::Generator->new(
                 pretty    => 2,
                 namespace => [HTML => "http://www.w3.org/TR/REC-html40"]);
    $pt = $html->html(
            $html->body(
              $html->font({ face => 'Arial' },
                          "Hello, there")));

::ok($pt,
'<HTML:html xmlns:HTML="http://www.w3.org/TR/REC-html40">
  <HTML:body>
    <HTML:font face="Arial">Hello, there</HTML:font>
  </HTML:body>
</HTML:html>');

    $html = XML::Generator->new(
                 pretty    => 2,
                 namespace => ["http://www.w3.org/TR/REC-html40"]);
    $pt = $html->html(
            $html->body(
              $html->font({ 'face' => 'Arial' },
                            "Hello, there")));

::ok($pt,
'<html xmlns="http://www.w3.org/TR/REC-html40">
  <body>
    <font face="Arial">Hello, there</font>
  </body>
</html>');


  my $a = XML::Generator->new(escape => 'always,high-bit');
  $pt = $a->foo("<\242>");

::ok($pt, '<foo>&lt;&#162;&gt;</foo>');

    $gen = XML::Generator->new(escape => 'always,apos');
    $pt = $gen->foo({'bar' => "It's all good"});

::ok($pt, '<foo bar="It&apos;s all good" />');

   $gen = XML::Generator->new(pretty => 2);
   $pt = $gen->foo($gen->bar('baz'),
                   $gen->qux({ tricky => 'no'}, 'quux'));

::ok($pt,
'<foo>
  <bar>baz</bar>
  <qux tricky="no">quux</qux>
</foo>');

   $gen = XML::Generator->new(namespace => [foo => "http://foo.com/"], qualifiedAttributes => 1);
   $pt = $gen->bar({baz => 3});

::ok($pt, '<foo:bar xmlns:foo="http://foo.com/" foo:baz="3" />');

   $pt = $gen->bar({'wow:baz' => 3});

::ok($pt, '<foo:bar xmlns:foo="http://foo.com/" wow:baz="3" />');

package TestMult;

$gen = XML::Generator->new(namespace => ['foo' => 'foo uri', 'bar' => 'bar uri']);
$pt = $gen->baz();
::ok($pt, '<foo:baz xmlns:foo="foo uri" xmlns:bar="bar uri" />');

$pt = $gen->bam(['#default' => 'default uri']);
::ok($pt, '<bam xmlns="default uri" />');

$pt = $gen->bam(['#default' => 'default uri', 'foo' => 'foo uri']);
::ok($pt, '<bam xmlns="default uri" xmlns:foo="foo uri" />');

$pt = $gen->bam(['foo' => 'foo uri', '#default' => 'default uri']);
::ok($pt, '<foo:bam xmlns:foo="foo uri" xmlns="default uri" />');

package TestRDF;

my @contact = (contact => "http://www.w3.org/2000/10/swap/pim/contact#");

$gen = XML::Generator->new(':pretty');
$pt = $gen->xml(
             $gen->RDF([ rdf     => "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                         @contact ],
                $gen->Person(\@contact, { 'rdf:about' => "http://www.w3.org/People/EM/contact#me" },
                  $gen->fullName(\@contact, 'Eric Miller'),
                  $gen->mailbox(\@contact, {'rdf:resource' => "mailto:em\@w3.org"}),
                  $gen->personalTitle(\@contact, 'Dr.'))));

::ok($pt, '<?xml version="1.0" standalone="yes"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:contact="http://www.w3.org/2000/10/swap/pim/contact#">
  <contact:Person rdf:about="http://www.w3.org/People/EM/contact#me">
    <contact:fullName>Eric Miller</contact:fullName>
    <contact:mailbox rdf:resource="mailto:em@w3.org" />
    <contact:personalTitle>Dr.</contact:personalTitle>
  </contact:Person>
</rdf:RDF>');

package TestEscapingEntities;

use XML::Generator escape => 'always,even-entities', conformance => 'strict', pretty => 2;

::ok(tag("&gt;"), '<tag>&amp;gt;</tag>');

package TestInvalidChars1;

use XML::Generator filter_invalid_chars => '1';

::ok(tag(map chr,
         0, 0x1, 0x8, 0xB, 0xC, 0xE..0x1F,
         0x7F..0x84, 0x86..0x9F), '<tag></tag>');

package TestInvalidCharsUnderStrict;

use XML::Generator ':strict';

::ok(tag("\0"), '<tag></tag>');

package TestInvalidCharsUnderStrict2;

use XML::Generator ':strict', 'filter_invalid_chars' => 0;

::ok(tag("\0"), "<tag>\0</tag>");

