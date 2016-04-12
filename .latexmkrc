$latex = $latex='platex -kanji=utf8 -guess-input-utf8 -synctex=1 -interaction=nonstopmode %S';
$bibtex = 'jbibtex';
# $dvipdf = $dvipdf='dvipdfmx -d 5 -f ptex-ipaex.map %S';
$dvipdf = $dvipdf='dvipdfmx -d 5 %S';
$dvips = 'pdvips';
$dvi_previewer = 'start xdvi';
$pdf_previewer = 'start evince';
$pdf_mode = 3;

# $latex = 'platex -kanji=utf8 -guess-input-enc -synctex=1 -interaction=nonstopmode %S';
# $bibtex = 'pbibtex';
# $dvipdf = 'dvipdfmx -p a4 -f ptex-ipaex.map -D "ps2pdf -dCompatibilityLevel=%v -sPAPERSIZE=a0    -dAutoFilterGrayImages=true -dGrayImageFilter=/DCTEncode -dEncodeGrayImages=true -dDownsampleGrayImages=true -dGrayImageDownsampleThreshold=1.5 -dGrayImageDownsampleType=/Bicubic -dGrayImageResolution=150    -dMonoImageFilter=/CCITTFaxEncode -dEncodeMonoImages=true -dDownsampleMonoImages=true -dMonoImageDownsampleThreshold=1.5 -dMonoImageDownsampleType=/Bicubic -dMonoImageResolution=300    -dAutoFilterColorImages=true -dColorImageFilter=/DCTEncode -dEncodeColorImages=true -dColorImageResolution=150 -dColorImageDownsampleThreshold=1.5 -dColorImageDownsampleType=/Bicubic    %i %o" %S';

# $dvips = 'pdvips';
# $dvi_previewer = 'start xdvi';
# $pdf_previewer = 'start evince';
# $pdf_mode = 3;