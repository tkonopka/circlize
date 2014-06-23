use strict;
use File::Temp qw/tempfile/;
use File::Basename;

my @Rscript = glob("example/*.R");

open HTML, ">index.html";

print HTML "
<html>
<head>
<title>Hello circlize</title>
<style>
img {
	width:300px;
}

#comment {
	border: 1px black solid;
	margin:10px 0px;
}
</style>

</head>
<body>
<h2>Examples of using <i>circlize</i></h2>
<table>";

my $i = 1;
foreach my $R (sort { (stat($b))[10] <=> (stat($a))[10] } @Rscript) {
	print "running $R\n";

	my $jpeg = $R;
	my $html = $R;
	$jpeg =~s/R$/jpg/;
	$html =~s/R$/html/;

	if($i % 4 == 1) {
		print HTML "<tr>";
	}
	
	print HTML "<td><a href='$html'><img src='$jpeg'/></a></td>";

	open HTML2, ">$html";
	print HTML2 "<html>
<head><title>$R</title>
<link rel='stylesheet' href='styles/github.css'>
<script src='highlight.pack.js'></script>
<script>hljs.initHighlightingOnLoad();</script>
</head><body><p><img src='".basename($jpeg)."' /></p>\n";
	print HTML2 "<p><pre><code>";
	open R, $R;
	my $comment = "";
	while(my $line = <R>) {

		if(/^##/) {
			$line =~s/^##\s*//;
			$comment .= "$line ";
			next;
		}
		$line =~s/\t/    /g;
		print HTML2 $line;
	}
	print HTML2 "<p id='comment'>$comment</p>\n";
	print HTML2 "</code></pre></p>\n</body></html>";
	close HTML2;
	close R;

	if($i % 4 == 0) {
		print HTML "</tr>\n";
	}

	$i ++;

	if(-e $jpeg) {
		next;
	}

	open R, $R;

	my ($fh, $filename) = tempfile();
	print $fh "jpeg('$jpeg', width = 600, height = 600)\n";
	while(<R>) {
		print $fh $_;
	}
	print $fh "\ndev.off()\n";
	close($fh);

	system("Rscript $filename");
	unlink($filename);
} 

if($i % 4 != 0) {
	print HTML "</tr>\n";
}

print HTML "</body></html>";