let x = 0; let y = 255; loop (100){clockwise 5; penrgb 255,y,x; loop (4){ahead 100; clockwise 90}; x += 4; y -= 4