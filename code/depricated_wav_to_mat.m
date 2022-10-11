my_dir = 'E:/SCREENS/Baseline/children/fix'
%output_dir_csv = 'C:/Users/eskovgaard/Desktop/sleep_study/data/raw/screens_csv_calibrated/baseline'
output_dir_mat = 'E:/screens_children_mat_files/baseline'

my_files = dir(fullfile(my_dir,'*.wav')); %gets all wav files in struct

for k = 1:(length(my_files))
    base_fileName = my_files(k).name;
    full_fileName = fullfile(my_dir, base_fileName);
    fprintf(1, 'Now reading %s\n', full_fileName);
    data = loadomwav2(full_fileName, []);

    %rawdatacsvfname = strrep(my_files(k).name,'.wav','_raw');
    %raw_filename = sprintf('%s/%s.csv',output_dir_csv,rawdatacsvfname)

    %tempdatacsvfname = strrep(my_files(k).name,'.wav','_temp');
    %temp_filename = sprintf('%s/%s.csv',output_dir_csv,tempdatacsvfname)

    mat_data = strrep(my_files(k).name,'.wav', '');
    mat_filename = sprintf('%s/%s.mat',output_dir_mat,mat_data);

    %writematrix(data.raw, raw_filename)
    %writematrix(data.temp, temp_filename)
    
    save(mat_filename, 'data')
end