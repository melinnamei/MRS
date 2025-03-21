% this is created by melinna to do the GABA analysis in glaucoma project on
% 2024/05/10. contact: melinna@live.cn

clear all;close all;clc;
tic;

% sub_code_list={'H63','H35','H59','H69','R18','H63','R75'}; %miss H63 t1w.nii.gz

all_subs=dir(['E:\A_MRI_UBSN\MRI\MRS_scan\data_check'])
sub_code_list={all_subs(3:end-1).name};


for ii=1:length(sub_code_list)
    sub_code=sub_code_list{ii};

    % set up the file
  %MRSCont = OspreyJob([pwd,filesep,'job_MEGA_all_single_GABA.m'],sub_code);
    MRSCont = OspreyJob([pwd,filesep,'job_MEGA_all.m'],sub_code);
  
 
    % do the load and process work
    MRSCont = OspreyLoad(MRSCont);
    MRSCont = OspreyProcess(MRSCont);

    %OspreyGUI(MRSCont);
    MRSCont = OspreyFit(MRSCont);
    MRSCont = OspreyCoreg(MRSCont);
    MRSCont = OspreySeg(MRSCont);

    MRSCont = OspreyQuantify(MRSCont);
    MRSCont = OspreyOverview(MRSCont);

    OspreyGUI(MRSCont);
    clear MRSCont;
    close all;

end
toc;