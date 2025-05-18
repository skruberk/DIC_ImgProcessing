// Macro that extracts particle x,y pos whatever else with frame number
//make sure that before you run this you go into set measurements and manually check
//the measurements you want, script relies on that
setBatchMode(true); //prevents updating after each frame so macro runs faster

//run("Set Measurements...", "area centroid perimeter shape redirect=None decimal=3");
// Create a custom table to store results
//table = Table.create("Particles with Frame Data"); //customTable = newArray("X", "Y", "Frame");  

// frame number
nFrames = nSlices;  

// open file *once* before the loop
file = File.open("/Users/kskruber/Documents/any.csv", "w"); // "w" for write, overwrites if exists
// Write the header row (only once)
//print(file, "area, Xpos,Ypos,Frame,perim,AR,Ellipse"); // Add a header row for clarity
print(file, "area,Xpos,Ypos,Frame,perim,AR,Major,Minor");

// loop over all frames in the stack
for (frame = 1; frame <= nFrames; frame++) {
    setSlice(frame);  // Set to the current frame
    //*****Important thresholding parameters: change that size number***********
    run("Analyze Particles...", "circularity=0.05-1.00 size=100-550 display exclude clear");  // run Analyze Particles
    // Get the number of results for the current frame
    nResult = nResults();
    
    // loop over all results for the current frame, add the frame number
    for (i = 0; i < nResult; i++) {
    	area = getResult("Area",i); //get
        xPos = getResult("X", i);  // get x
        yPos = getResult("Y", i);  // get y
        perim=getResult("Perim.",i); // get perim
        aspectRatio = getResult("AR", i);// get ar
        major = getResult("Major", i);
		minor = getResult("Minor", i);

        //fit = getResult("Major", i); //get fit ellipse
        //fit = getResult("Minor", i); //get fit ellipse
        
        // write to file within loop
        //print(file, area + "," + xPos + "," + yPos + "," + frame + "," + perim + "," + aspectRatio "," + fit );  // comma-separated values
		print(file, area + "," + xPos + "," + yPos + "," + frame + "," + perim + "," + aspectRatio + "," + major + "," + minor);

        // Add X, Y, and Frame number to the results table
        setResult("Area", i, area);  // Update column
        setResult("X", i, xPos);  // Update X column
        setResult("Y", i, yPos);  // Update Y column
        setResult("Frame", i, frame);  // Add Frame column 
        setResult("Perim.",i, perim);
        setResult("AR",i, aspectRatio);
        setResult("Major",i, major);
        setResult("Minor",i, minor); 
    }
    updateResults();  // update results table
 }    
    
// Display the overlay on the current image
//run("Show All", "");  // Show the overlay for all frames in the stack
setBatchMode(false); // restore
// Show the table turn off after 
//Table.showArrays();
// Save the Results Table to a CSV file
saveAs("Results", "/Users/kskruber/Documents/frames.csv");  // save as csv