pacman::p_load(readxl, ggplot2, ggthemes, tidyverse, readr, scales)

#### Board Presentation Graphics




## Import the data
path <- "C:/Users/mneff/Desktop/College Stuff/Fall 2019/Business Fundamentals/Board Presentation/Board Presentation Data.xlsx"

sales_data <- read_excel(path = path, range = "A2:G9")
net_income_data <- read_excel(path = path, range = "A13:G20")
stock_price_data <- read_excel(path = path, range = "A24:G31")
cap_invstmt_data <- read_excel(path = path, range = "A59:G67")
andrews_data <- read_excel(path = path, range = "A46:G54")


## Tidy the Data
sales_tidy <- sales_data %>% 
  gather(key = "Company", value = "Sales", 2:7) %>% 
  group_by(Company)

net_income_tidy <- net_income_data %>% 
  gather(key = "Company", value = "Net_Income", 2:7) %>% 
  group_by(Company)

stock_price_tidy <- stock_price_data %>% 
  gather(key = "Company", value = "Stock_Price", 2:7) %>% 
  group_by(Company)

cap_invstmt_tidy <- cap_invstmt_data %>% 
  gather(key = "Company", value = "Capital_Investment", 2:7) %>% 
  group_by(Company)

# For graphing the Sales and Net Income
income_and_sales <- left_join(sales_tidy, net_income_tidy, by = c("Round", "Company")) %>% 
  filter(Company %in% c("Andrews","Chester")) %>% 
  group_by(Company)


## Endpoints for labelling the companies
sales_end <- sales_tidy %>% 
  filter(Round == 7)

net_income_end <- net_income_tidy %>% 
  filter(Round == 7)

stock_price_end <- stock_price_tidy %>% 
  filter(Round == 7)

cap_end <- cap_invstmt_tidy %>% 
  filter(Round == 7)

profit_end <- income_and_sales %>% 
  filter(Round == 5 & Company == "Andrews")

total_sale_end <- income_and_sales %>% 
  filter(Round == 5 & Company == "Andrews")

# Adjust specific data points
sales_end$Sales[1] <- sales_end$Sales[1] + 1000 # Andrews Sales up
sales_end$Sales[3] <- sales_end$Sales[3] - 1000 # Chester Sales down


## Plot and Save the Data


# Net Income and Sales - Andrews and Chester
ggplot(income_and_sales) + 
  geom_ribbon(aes(x = Round, ymax = Sales, ymin = Net_Income, group = Company), 
              fill = "lightsteelblue1", alpha = 0.5, show.legend = FALSE) + 
  geom_ribbon(aes(x = Round, ymax = Net_Income, ymin = 0, group = Company), 
              fill = "green", alpha = 0.5, show.legend = FALSE) + 
  # Labels to indicate which area is Sales and which is Net Income
  geom_text(data = profit_end, 
            aes(x = Round, 
                y = Net_Income + 5000,
                label = "Net Income"),
            color = "green2",
            nudge_x = -.5) + 
  geom_text(data = total_sale_end, 
            aes(x = Round, 
                y = Sales + 4000,
                label = "Sales"),
            color = "lightsteelblue3",
            nudge_x = -.3) + 
  facet_grid(.~Company) + 
  scale_x_continuous(breaks = seq(1, 7, by = 2)) + 
  scale_y_continuous(labels = dollar_format(big.mark = ",", suffix = " M", scale = 1/1000)) + 
  labs(title = "Overall Profit") + 
  theme_economist_white() + 
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(size = 0.2, linetype = "dotted", color = "gray90"),
        axis.title.y = element_blank())
ggsave("profit.png", width = 19, height = 10, path = ".\\Board Presentation")


# Sales
ggplot(sales_tidy) + 
  geom_path(aes(x = Round, y = Sales, group = Company, color = Company, size = Company, linetype = Company), 
            show.legend = FALSE, lineend = "round") + 
  geom_text(data = sales_end,
            aes(x = Round, 
                y = Sales,
                label = Company,
                color = Company),
            nudge_x = .03,
            hjust = 0,
            show.legend = FALSE) + 
  coord_cartesian(clip = "off") + 
  scale_color_manual(values = c("lightskyblue","gray80","gray70","gray80","gray80","gray80")) +
  scale_size_manual(values = c(2, .75, 1, .75, .75, .75)) + 
  scale_alpha_manual(values = c(1, .2, .5, .2, .2, .2)) + 
  scale_linetype_manual(values = c("solid","dashed","solid","dashed","dashed","dashed")) + 
  scale_x_continuous(breaks = seq(1, 7, by = 1)) + 
  scale_y_continuous(labels = dollar_format(big.mark = ",")) + 
  labs(y = "Sales (in Millions)", title = "Market Sales") + 
  theme_clean()
ggsave("sales.png", width = 13, height = 5.8, path = ".\\Board Presentation")


# Net Income
ggplot(net_income_tidy) + 
  geom_path(aes(x = Round, y = Net_Income, group = Company, color = Company, size = Company, linetype = Company), 
            show.legend = FALSE, lineend = "round") + 
  geom_text(data = net_income_end,
            aes(x = Round, 
                y = Net_Income,
                label = Company,
                color = Company),
            nudge_x = .03,
            hjust = 0,
            show.legend = FALSE) + 
  coord_cartesian(clip = "off") + 
  scale_color_manual(values = c("lightskyblue","gray80","gray70","gray80","gray80","gray80")) +
  scale_size_manual(values = c(2, .75, 1, .75, .75, .75)) + 
  scale_alpha_manual(values = c(1, .2, .5, .2, .2, .2)) + 
  scale_linetype_manual(values = c("solid","dashed","solid","dashed","dashed","dashed")) + 
  scale_x_continuous(breaks = seq(1, 7, by = 1)) + 
  scale_y_continuous(labels = dollar_format(big.mark = ",")) + 
  labs(y = "Net Income (in Millions)", title = "Market Net Income") + 
  theme_clean()
ggsave("net_income.png", width = 13, height = 5.8, path = ".\\Board Presentation")


# Stock Price
ggplot(stock_price_tidy) + 
  geom_path(aes(x = Round, y = Stock_Price, group = Company, color = Company, size = Company, linetype = Company), 
            show.legend = FALSE, lineend = "round") +  
  geom_text(data = stock_price_end,
            aes(x = Round, 
                y = Stock_Price,
                label = Company,
                color = Company),
            nudge_x = .03,
            hjust = 0,
            show.legend = FALSE) + 
  coord_cartesian(clip = "off") + 
  scale_color_manual(values = c("lightskyblue","gray80","gray70","gray80","gray80","gray80")) +
  scale_size_manual(values = c(2, .75, 1, .75, .75, .75)) + 
  scale_alpha_manual(values = c(1, .2, .5, .2, .2, .2)) + 
  scale_linetype_manual(values = c("solid","dashed","solid","dashed","dashed","dashed")) + 
  scale_x_continuous(breaks = seq(1, 7, by = 1)) + 
  scale_y_continuous(labels = dollar_format(big.mark = ",")) + 
  labs(y = "Closing Stock Price (in Millions)", title = "Market Stock Prices") + 
  theme_clean()
ggsave("stock_price.png", width = 18, height = 10, path = ".\\Board Presentation")


# Capital Investment
ggplot(cap_invstmt_tidy) + 
  geom_path(aes(x = Round, y = Capital_Investment, 
                group = Company, color = Company, size = Company, linetype = Company), 
            show.legend = FALSE, lineend = "round") + 
  geom_text(data = cap_end,
            aes(x = Round, 
                y = Capital_Investment,
                label = Company,
                color = Company),
            nudge_x = .03,
            hjust = 0,
            show.legend = FALSE) + 
  coord_cartesian(clip = "off") + 
  scale_color_manual(values = c("lightskyblue","gray80","gray70","gray80","gray80","gray80")) +
  scale_size_manual(values = c(2, .75, 1, .75, .75, .75)) + 
  scale_alpha_manual(values = c(1, .2, .5, .2, .2, .2)) + 
  scale_linetype_manual(values = c("solid","dashed","solid","dashed","dashed","dashed")) + 
  scale_x_continuous(breaks = seq(1, 7, by = 1)) + 
  scale_y_continuous(labels = dollar_format(big.mark = ",")) + 
  labs(y = "Capital Investment (in Millions)", title = "Market Capital Investment") + 
  theme_clean()
ggsave("capital_investment.png", width = 18, height = 10, path = ".\\Board Presentation")


# Andrew Stats
ggplot(andrews_data) + 
  geom_line(aes(x = Round, y = ROS)) + 
  geom_line(aes(x = Round, y = ROA)) + 
  geom_line(aes(x = Round, y = ROE)) 
  
  
  
