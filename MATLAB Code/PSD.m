samplingRate = 250;

lowerFrequency = 8;
upperFrequency = 12;

%computing log spectrum for different frequencies
[power, freq] = spectopo(EEG.data(8, :), 0, samplingRate, 'winsize', 256, 'overlap', 128);

%average power within the predefined frequency range
meanPower = mean(power(freq >= lowerFrequency & freq <= upperFrequency));

disp(meanPower)