%% === Settings ===
folder = pwd;               % Current working directory
fileName = 'MASK_57C_4.tif'; % Your multi-frame TIFF
outputFile = 'colorcoded_outlines.png';
outlineThickness = 1;       % >1 for thicker outlines
numColors = 16;             % LUT size (16 colors)
minArea = 7500;             % Minimum object size in pixels

%% === Read TIFF Info ===
filePath = fullfile(folder, fileName);
info = imfinfo(filePath);
numFrames = numel(info);
fprintf('Found %d frames in %s\n', numFrames, fileName);

%% === Create 16-Color LUT ===
cmap = jet(numColors);   % Options: parula, hot, hsv, etc.
cmap = flipud(cmap);     % Optional: reverse color order

%% === Read first frame to get size ===
sample = imread(filePath, 1);
[rows, cols] = size(sample);

%% === Initialize RGB composite ===
outlineRGB = zeros(rows, cols, 3);

%% === Process Each Frame ===
for i = 1:numFrames
    mask = imread(filePath, i);
    mask = mask > 0; % Ensure binary
    
    % === Remove small objects but keep all above threshold ===
    mask = bwareaopen(mask, minArea);
    
    if ~any(mask(:))
        continue; % Skip frame if no objects remain
    end
    
    % Extract outline
    outline = bwperim(mask);
    
    % Optional: thicken outline
    if outlineThickness > 1
        outline = imdilate(outline, strel('disk', outlineThickness));
    end
    
    % Determine color based on frame index
    colorIdx = min(ceil((i/numFrames) * numColors), numColors);
    color = cmap(colorIdx, :);
    
    % Add colored outline to composite
    for ch = 1:3
        outlineRGB(:,:,ch) = outlineRGB(:,:,ch) + outline * color(ch);
    end
end

%% === Normalize Composite ===
outlineRGB(outlineRGB > 1) = 1; % Cap at 1

% Invert background to white:
outlineRGB = 1 - outlineRGB;
outlineRGB(outlineRGB < 0) = 0;

%% === Display Result ===
figure; imshow(outlineRGB);
title('Color-coded Outlines (16-color LUT)');

%% === Add Color Legend ===
figure;
colormap(cmap);
colorbar('Ticks', linspace(0, 1, numColors), ...
         'TickLabels', round(linspace(1, numFrames, numColors)));
title('Frame Index → Color Mapping');

%% === Save Output ===
imwrite(outlineRGB, outputFile);
fprintf('Saved color-coded outline image to %s\n', outputFile);
