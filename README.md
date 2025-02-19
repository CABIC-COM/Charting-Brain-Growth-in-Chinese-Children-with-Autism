# **CABIC Project Replication Code Repository**

## **Introduction**  
This GitHub repository contains the necessary code to replicate the **CABIC project**.  
**Note:** This repository **does not include any datasets**.  

The **Lifespan Brain Chart Consortium (LBCC)** has recently developed a comprehensive framework for mapping **nonlinear trajectories of human brain morphology** across the lifespan and has publicly shared their code.  
**Acknowledgment:** Our analysis is built upon their framework, and we appreciate LBCC’s support.  

---

## **Pipeline Overview & Script Descriptions**  

### **1. CABIC_data.R**  
- **1.1** Converts demographic data from `.csv` format into `.rds` format for easier computation and performs data aggregation.  
- **1.2** Specifies the output type, such as **GMV (Gray Matter Volume)** or **WMV (White Matter Volume)**.  
- **1.3** Uses the **Combat method** to harmonize multi-site training data.  
- **1.4** Generates model datasets for further analysis.  

### **2. CABIC_Main.R**  
- **2.1** Trains models using datasets and parameters defined in `CABIC_data.R`.  
- **2.2** Builds and saves the trained models.  

### **3. CABIC_Drawing.R**  
- **3.1** Uses the models built in `CABIC_Main.R` to plot **growth trajectory curves**.  
- **3.2** Visualizes and compares **developmental trajectories** between ASD (Autism Spectrum Disorder) and TDC (Typically Developing Children).  

### **4. CABIC_milestone.R**  
- **4.1** Tracks key milestones in the CABIC project workflow.  

### **5. CABIC_Subregions_Main.R**  
- **5.1** Similar to Step 2, developmental curves for each brain region were plotted using gray matter volume values for local brain regions  

### **6. CABIC_Normalised Centiles.R**  
- **6.1** Deviation from the standardized developmental curve compared to healthy controls was calculated for each input participant

---

## **How to Use**

### **Installation**
1. Clone the repository:
   ```sh
   git clone https://github.com/yourusername/CABIC_project.git
   cd CABIC_project
   ```
2. Install necessary dependencies (if applicable):
   ```sh
   Rscript -e 'install.packages(c("necessary_package1", "necessary_package2"))'
   ```

### **Running the Pipeline**
1. Preprocess data:
   ```sh
   Rscript CABIC_data.R
   ```
2. Train the model:
   ```sh
   Rscript CABIC_Main.R
   ```
3. Generate plots:
   ```sh
   Rscript CABIC_Drawing.R
   ```
4. Track milestones:
   ```sh
   Rscript CABIC_milestone.R
   ```
5. Evaluate model performance:
   ```sh
   Rscript CABIC_Evaluation.R
   ```
6. Generate report:
   ```sh
   Rscript CABIC_Report.R
   ```

---

## **License**
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.  

## **Contact**
For any inquiries, please contact **Lei Li** at `uestc.lilei@gmail.com`.  
