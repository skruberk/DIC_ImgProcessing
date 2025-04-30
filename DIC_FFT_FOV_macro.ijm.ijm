input = getDirectory("Choose an input directory");
output = getDirectory("Choose an output directory");

run("Bio-Formats Macro Extensions"); // Load Bio-Formats extensions

list = getFileList(input);

for (i = 0; i < list.length; i++) {
    if (endsWith(list[i], ".ome.tif")) {
        processFile(input, output, list[i]);
    }
}

function processFile(input, output, file) {
    path = input + file;

    run("Bio-Formats Macro Extensions");
    Ext.setId(path); // Set ID properly
    //seriesCount = Ext.getSeriesCount(); // Get number of series

    // === Correctly remove extension from filename ===
    dotIndex = lastIndexOf(file, ".");
    if (dotIndex > 0)
        title2 = substring(file, 0, dotIndex);
    else
        title2 = file;

    for (s = 0; s < 3; s++) {
        run("Bio-Formats Importer", "open=[" + path + "] series=" + s + " stack_order=XYCZT view=Hyperstack");

        currentTitle = getTitle();

        output2 = output + File.separator + title2 + "_series" + (s+1);
        File.makeDirectory(output2);

        run("Enhance Contrast", "saturated=0.35");
        run("Grays");
        run("Bandpass Filter...", "filter_large=50 filter_small=5 suppress=None tolerance=5 autoscale saturate process");

        saveAs("Tiff", output2 + File.separator + "55C_" + title2 + "_series" + (s+1) + ".tif");
		//selectWindow(currentTitle);
        //close(currentTitle);
        close("*");
    }
}

