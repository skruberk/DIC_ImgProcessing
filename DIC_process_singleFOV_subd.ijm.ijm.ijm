// Ask user for input and output folders
input = getDirectory("Choose an input directory");
output = getDirectory("Choose an output directory");

run("Bio-Formats Macro Extensions"); // Load Bio-Formats extensions

// Start processing from top-level folder
processFolder(input);

// Recursively process folders
function processFolder(input) {
    list = getFileList(input);
    for (i = 0; i < list.length; i++) {
        if (endsWith(list[i], ".ome.tif")) {
            processFile(input, output, list[i]);
        } else if (endsWith(list[i], "/")) {
            processFolder(input + list[i]); // recursive call
        } else {
            print("Skipping: " + input + list[i]);
        }
    }
}

// File processing function
function processFile(input, output, file) {
    path = input + file;

    // Remove extension from filename
    dotIndex = lastIndexOf(file, ".");
    if (dotIndex > 0)
        title2 = substring(file, 0, dotIndex);
    else
        title2 = file;

    // Import image
    run("Bio-Formats Importer", "open=[" + path + "] color_mode=Default view=Hyperstack stack_order=XYCZT");
    currentTitle = getTitle();

    // Create output subdirectory
    output2 = output + File.separator + title2;
    File.makeDirectory(output2);

    // Example processing
    run("Enhance Contrast", "saturated=0.35");
    run("Grays");
    run("Bandpass Filter...", "filter_large=50 filter_small=5 suppress=None tolerance=5 autoscale saturate process");

    // Save result
    saveAs("Tiff", output2 + File.separator + "pxd_" + title2 + ".tif");

    // Close everything
    close("*");
}
