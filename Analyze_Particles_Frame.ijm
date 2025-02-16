// Macro that extracts particle x,y pos whatever else with frame number

setBatchMode(true) //prevents updating after each frame so macro runs faster

run("Set Measurements...", "centroid perimeter shape redirect=None decimal=3");
// Create a custom table to store results
table = Table.create("Particles with Frame Data"); //customTable = newArray("X", "Y", "Frame");  

// frame number
nFrames = nSlices;  

// Open the file *once* before the loop.  This is crucial!
file = File.open("/Users/kskruber/Documents/any.csv", "w"); // "w" for write, overwrites if exists
// Write the header row (only once)
print(file, "Xpos,Ypos,Frame,perim,AR"); // Add a header row for clarity

// Loop over all frames in the stack
for (frame = 1; frame <= nFrames; frame++) {
    setSlice(frame);  // Set to the current frame
    run("Analyze Particles...", "size=16-75 display exclude clear");  // run Analyze Particles
    // Get the number of results for the current frame
    nResult = nResults();
    
    // Loop over all results for the current frame and add the frame number
    for (i = 0; i < nResult; i++) {
        xPos = getResult("X", i);  // Get X position
        yPos = getResult("Y", i);  // Get Y position
        perim=getResult("perimeter",i);
        aspectRatio = getResult("AR", i);// Get Aspect Ratio
        
        // write to file within loop
        print(file, xPos + "," + yPos + "," + frame + "," + perim + "," + aspectRatio );  // comma-separated values
        // Add X, Y, and Frame number to the results table
        
        setResult("X", i, xPos);  // Update X column
        setResult("Y", i, yPos);  // Update Y column
        setResult("Frame", i, frame);  // Add Frame column 
        setResult("perimeter",i, perim);
        setResult("AR",i, aspectRatio);
        
    }
    updateResults();  // Update the results table with the new data
 }    
    
// Display the overlay on the current image
//run("Show All", "");  // Show the overlay for all frames in the stack
setBatchMode(false); // restore
// Show the table turn off after 
//Table.showArrays();
// Save the Results Table to a CSV file
saveAs("Results", "/Users/kskruber/Documents/frames.csv");  // save as csv