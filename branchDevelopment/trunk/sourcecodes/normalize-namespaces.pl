use strict;

open TOP, "<obi.owl" or die("can't find obi.owl");
my @lines = <TOP>;
close TOP;

my $namespaces = join ("",<DATA>);

my $copydir = "newns/";

# extract the names of the branches

my @obiParts = grep {/<owl:imports rdf:resource="http:\/\/obi.sourceforge.net\/ontology\/OBI\/(.*)\.owl"/;$1} @lines;
my @obiParts = map { s/.*\/([A-Za-z.-]*)\.owl.*/$1/;chomp $_ ; $_} @obiParts;

sub replaceNamespaces
  {

    foreach my $part ( @obiParts) {
      my $path = "$part".".owl";
      open PART, "<$path" or die("Trouble loading $path");
      my $copypath = $copydir.$part.".owl";
      open PARTCOPY, ">$copypath" or die ("Trouble writing $path");
      my $rewrite=1;
      while (<PART>) {
	  if (/xml:base/)
	  { print PARTCOPY $namespaces;
	    $rewrite=0
	  }
	  elsif ($rewrite) { next if /^\s*xmlns/ || /^\s*$/ }
	  print PARTCOPY $_;
      }
      close PART;
      close PARTCOPY;
    }
  }

replaceNamespaces();

$DB::single=1;1;

__DATA__
   xmlns:obi_rest="http://obi.sourceforge.net/ontology/OBI/TheRest.owl#"
   xmlns:obi_obsolete="http://obi.sourceforge.net/ontology/OBI/Obsolete.owl#"
   xmlns:obi_func="http://obi.sourceforge.net/ontology/OBI/OBI-Function.owl#"
   xmlns:obi_biomat="http://obi.sourceforge.net/ontology/OBI/Biomaterial.owl#"
   xmlns:obi_denrie="http://obi.sourceforge.net/ontology/OBI/DigitalEntityPlus.owl#"
   xmlns:obi_rel="http://obi.sourceforge.net/ontology/OBI/Relations.owl#"
   xmlns:obi_plan="http://obi.sourceforge.net/ontology/OBI/PlanAndPlannedProcess.owl#"
   xmlns:obi_annot="http://obi.sourceforge.net/ontology/OBI/AnnotationProperty.owl#"
   xmlns:obi_data_trans="http://obi.sourceforge.net/ontology/OBI/DataTransformation.owl#"
   xmlns:obi_quality="http://obi.sourceforge.net/ontology/OBI/Quality.owl#"
   xmlns:obi_role="http://obi.sourceforge.net/ontology/OBI/Role.owl#"
   xmlns:obi_instr="http://obi.sourceforge.net/ontology/OBI/InstrumentAndPart.owl#"
   xmlns:obi_ext="http://obi.sourceforge.net/ontology/OBI/external.owl#"
   xmlns:obi_extd="http://obi.sourceforge.net/ontology/OBI/externalDerived.owl#"
   xmlns:obi_owlfull="http://obi.sourceforge.net/ontology/OBI/obi-owl-full.owl#"
   xmlns:ro="http://www.obofoundry.org/ro/ro.owl#"
   xmlns:rotoo="http://purl.org/obo/owl/ro#"
   xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
   xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
   xmlns:owl="http://www.w3.org/2002/07/owl#"
   xmlns:dc="http://purl.org/dc/elements/1.1/"
   xmlns:dcterms="http://purl.org/dc/terms/"
   xmlns:protege-dc="http://protege.stanford.edu/plugins/owl/dc/protege-dc.owl#"
   xmlns:bfo="http://www.ifomis.org/bfo/1.1#"
   xmlns:robfo="http://purl.org/obo/owl/ro_bfo1-1_bridge#"
   xmlns:snap="http://www.ifomis.org/bfo/1.1/snap#"
   xmlns:span="http://www.ifomis.org/bfo/1.1/span#"
   xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
   xmlns:pato="http://purl.org/obo/owl/PATO#"
   xmlns:cell="http://purl.org/obo/owl/CL#"
   xmlns:protege="http://protege.stanford.edu/plugins/owl/protege#"
   xmlns:go="http://www.geneontology.org/formats/oboInOwl#"
   xmlns:obi="http://obi.sourceforge.net/ontology/OBI.owl#"
   xmlns="http://obi.sourceforge.net/ontology/OBI.owl#"
