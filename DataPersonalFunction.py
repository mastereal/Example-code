#ignored
import os
import re
import pandas as pd
from pandas import DataFrame, Series
import numpy as np

def Difference_2DataFrame(dataframe_a,dataframe_b,type=0):
    """两个格式相同的表格，数值相减.

    将两个表格中的column index和row index提取出来建立新的，所有值为0的空白表格
    让新表每个单元格的值，等于两个旧表对应单元格的值之差。
    两个表格必须有同样的column index和row index

    Args:
        dataframe_a:第一张表格
        dataframe_b:第二张表格
    
    Returns:
        一张值等于dataframe_a减去dataframe_b的新表
    """
    if type==0:
        acolumns=list(dataframe_a.columns.str.strip())
        bcolumns=list(dataframe_b.columns.str.strip())
        aindex=list(dataframe_a.index)
        bindex=list(dataframe_b.index)
        print(acolumns)
        print(bcolumns)
        print(aindex)
        print(bindex)
        df=pd.DataFrame(columns=acolumns,index=aindex) 
        df=df.fillna(0) #制表
        for row in acolumns:
            for ind in aindex:
                na=acolumns.index(row)
                ia=aindex.index(ind)
                nb=bcolumns.index(row)                
                df.iloc[ia,na]=dataframe_a.iloc[ia,na]-dataframe_b.iloc[ia,nb]
        return df

def three_merge(file1,file2,file3,key1,key2):
    """合并3张csv表

    所有的表中都以key1,key2为主键（panel data[id,year]）(df[key1,key2])

    Args:
        file1:第一张表格
        file2:第二张表格
        file3:第三张表格
        key1:第一个主键的名称字串符
        key2:第二个主键的名称字串符
    
    Returns:
        合并后以key1,key2为主键的表，如果三张表都有同名列，则_x是file2的列，_y是file3的列
    """
    filename1=file1+".csv"
    filename2=file2+".csv"
    filename3=file3+".csv"
    df1=pd.read_csv(filename1)
    df2=pd.read_csv(filename2)
    df3=pd.read_csv(filename3)
    dfa=pd.merge(df1,df2,how="left",on=[key1,key2])
    dfb=pd.merge(dfa,df3,how="left",on=[key1,key2])
    return dfb

def three_merge_df(df1,df2,df3,key1,key2):
    """合并3张Dataframe表

    所有的表中都以key1,key2为主键（panel data[id,year]）(df[key1,key2])

    Args:
        file1:第一张df
        file2:第二张df
        file3:第三张df
        key1:第一个主键的名称字串符
        key2:第二个主键的名称字串符
    
    Returns:
        合并后以key1,key2为主键的表，如果三张表都有同名列，则_x是file1的列，_y是file2的列
    """
    dfa=pd.merge(df1,df2,how="left",on=[key1,key2])
    dfb=pd.merge(dfa,df3,how="left",on=[key1,key2])
    return dfb


class Intercut:
    """Intercut(start,lim)表示建立一个区间在[start,lim]之间的迭代器实例

    提供一个以0.01为间隔的递增迭代器，从方法给定的起始端start开始，到lim结束，，此时实例的含义是迭代器
    iter是触发迭代的方法，iter(实例)之后，有一个可供迭代的变量从初始值开始迭代，此时实例的含义是迭代器中一个可供迭代的量
    next是执行迭代的方法，next(实例)之后，可供迭代的变量向后迭代一单位间隔一次，此时实例的含义是迭代器中一个迭代中的量
    所以只有在iter方法使用后，才有可供迭代的变量产生，才能执行next方法。

    Attributes:
        self.start：迭代器区间的最小端点
        self.lim：迭代器区间的最大端点
        self.iter_value: 迭代中的值

    """
    def __init__(self,start=0,lim=1000): #迭代的默认次数从0到1000
        self.start=start
        self.lim=lim
    def __iter__(self): #设置Intercut的iter方法
        self.iter_value = self.start #初始迭代值等于start
        return self
 
    def __next__(self): #设置Intercut的next方法,
        if self.iter_value <= self.lim:
            x = self.iter_value
            self.iter_value +=0.01
            return x
        else:
            raise StopIteration #增加一种报错，并停止迭代

def grade_index(dataframe_DW,DW_column_name,DW_column_cat):
    """根据县次划分旱涝等级

    Dryness/Wetness grade 根据所输入的旱涝距平百分比之差的分布，设定旱涝等级

    Args:
        dataframe_DW:包含旱涝距平百分比之差的时间序列表格
        DW_column_name:包含旱涝距平百分比之差的时间序列地点或编号的列名称
        DW_column_cat:包含旱涝距平百分比之差的时间序的列名称
    
    Returns:
        返回包含地点名称，年份，旱涝等级的表格
    """

    x=dataframe_DW[DW_column_cat] # 导入旱涝距平百分比之差的时间序列
    maxv=x.max() 
    minv=x.min() 
    mn=[abs(maxv),abs(minv)] #找到时间序列中的最小值和最大值的绝对值
    bound=np.float(max(mn))+1 #在所有绝对之中，找到最大的那个，+1后作为最大边界
    l=np.float(len(x)) #时间序列的总长度
 

    #print(bound,l)
    bins=[]
    a=iter(Intercut(0,bound)) #建立一个迭代器开始从0迭代

    while True:
        t0=next(a) # 随着循环每次迭代0.01的值
        if pd.value_counts(pd.cut(x,[0-t0,t0]))[0]/l>=0.35: #pd.cut将x按照[]中的大小顺序划分点分割，pd.value_counts对每段分割进行计数，如果中间段的数量占比在全部分布中不超过35%，则重复迭代
            t1=t0+0.01
            bins.append(t1) 
            break #如果超过了35%，则记录迭代的终值并跳出循环
    #print(bins[0]) #上个迭代的终值
    b=iter(Intercut(bins[0],bound)) #建立新的迭代器，初值从上个迭代的终值开始
    #print(next(b))
    while True:
        try:
            t0=next(b)
        except StopIteration: #如果出现迭代中断，说明迭代到了最大端点，如果没到最大端点，则继续if语句
            df=DataFrame()
            Tbins=[0-bound,0-bins[0],bins[0],bound]
            columnname="I_"+DW_column_cat
            df[columnname]=pd.cut(x,Tbins,labels=["3","4","5"])
            df[DW_column_name]=dataframe_DW[DW_column_name] 
            df["Year"]=dataframe_DW["Year"]   
            print(pd.value_counts(pd.cut(x,Tbins)))
            return df

        if pd.value_counts(pd.cut(x,[0-t0,-bins[0]]))[0]/l+pd.value_counts(pd.cut(x,[bins[0],t0]))[0]/l>0.4: #3，5级占比不超过40%时重复迭代
            t1=t0
            bins.append(t1)
            break
    c=iter(Intercut(bins[1],bound))
    while True:
        try:
            t0=next(c)
        except StopIteration:
            df=DataFrame()
            Tbins=[0-bound,0-bins[1],0-bins[0],bins[0],bins[1],bound]
            columnname="I_"+DW_column_cat
            df[columnname]=pd.cut(x,Tbins,labels=["2","3","4","5","6"])
            df[DW_column_name]=dataframe_DW[DW_column_name] 
            df["Year"]=dataframe_DW["Year"]   
            print(pd.value_counts(pd.cut(x,Tbins)))
            return df
        if pd.value_counts(pd.cut(x,[0-t0,-bins[1]]))[0]/l+pd.value_counts(pd.cut(x,[bins[1],t0]))[0]/l>0.2: #2，6级不超过20%
            t1=t0
            #print(t1,bound)
            print(pd.value_counts(pd.cut(x,[0-bound,0-t1]))[0]/l+pd.value_counts(pd.cut(x,[t1,bound]))[0]/l)
            #print(pd.value_counts(pd.cut(x,[t1,bound]))[0]/l)
            bins.append(t1)
            break        
    df=DataFrame()
    Tbins=[0-bound,0-bins[2],0-bins[1],0-bins[0],bins[0],bins[1],bins[2],bound]
    columnname="I_"+DW_column_cat
    try:
        df[columnname]=pd.cut(x,Tbins,labels=["1","2","3","4","5","6","7"])
        df[DW_column_name]=dataframe_DW[DW_column_name] 
        df["Year"]=dataframe_DW["Year"]   
        print(pd.value_counts(pd.cut(x,Tbins)))
        return df
    except ValueError:
        df[columnname]=pd.cut(x,Tbins,labels=["2","3","4","5","6"],duplicates="drop")
        df[DW_column_name]=dataframe_DW[DW_column_name] 
        df["Year"]=dataframe_DW["Year"]   
        print(pd.value_counts(pd.cut(x,Tbins)))
        return df

def alter_grade(dataframe_DW,DW_column_name,DW_column_cat):
    """根据县次划分旱涝等级,另一种方法

    Dryness/Wetness grade 根据所输入的旱涝距平百分比之差的分布，设定旱涝等级，不采用迭代法寻找范围，而是逐个判断分位数法。

    Args:
        dataframe_DW:包含旱涝距平百分比之差的时间序列表格
        DW_column_name:包含旱涝距平百分比之差的时间序列地点或编号的列名称
        DW_column_cat:包含旱涝距平百分比之差的时间序的列名称
    
    Returns:
        返回包含地点名称，年份，旱涝等级的表格
    """

    x=dataframe_DW[DW_column_cat] # 导入旱涝距平百分比之差的时间序列
    l=np.float(len(x)) #时间序列的总长度
    columnname="I_"+DW_column_cat
    maxv=x.max() 
    minv=x.min() 
    mn=[abs(maxv),abs(minv)] #找到时间序列中的最小值和最大值的绝对值
    bound=np.float(max(mn))+1 #在所有绝对之中，找到最大的那个，+1后作为最大边界
    gradelist=[]
    for value in x:
        if pd.value_counts(pd.cut(x,[-bound,value]))[0]/l>=0.975:
            gradelist.append(7)
        elif pd.value_counts(pd.cut(x,[-bound,value]))[0]/l>=0.875 and pd.value_counts(pd.cut(x,[-bound,value]))[0]/l<0.975:
            gradelist.append(6)
        elif pd.value_counts(pd.cut(x,[-bound,value]))[0]/l>=0.675 and pd.value_counts(pd.cut(x,[-bound,value]))[0]/l<0.875:
            gradelist.append(5)
        elif pd.value_counts(pd.cut(x,[-bound,value]))[0]/l>=0.325 and pd.value_counts(pd.cut(x,[-bound,value]))[0]/l<0.675:
            gradelist.append(4)
        elif pd.value_counts(pd.cut(x,[-bound,value]))[0]/l>=0.125 and pd.value_counts(pd.cut(x,[-bound,value]))[0]/l<0.325:
            gradelist.append(3)
        elif pd.value_counts(pd.cut(x,[-bound,value]))[0]/l>=0.025 and pd.value_counts(pd.cut(x,[-bound,value]))[0]/l<0.125:
            gradelist.append(2)
        elif pd.value_counts(pd.cut(x,[-bound,value]))[0]/l<0.025:
            gradelist.append(1)
    df=DataFrame()
    df[columnname]=gradelist
    df[DW_column_name]=dataframe_DW[DW_column_name] 
    df["Year"]=dataframe_DW["Year"]
    return df

def informal_grade(value):
    """根据z值判断分位数区间，设定旱涝等级

    Args:
        value:z值

    Returns:
        返回z值所对应的旱涝等级
    """

    #zlist=[-1.96,-1.15,-0.45,0.45,1.15,1.96]
    if np.isnan(value)==True:
        return value
    else:
        if value>=1.96:
            return 7
        elif 1.15<value<1.96:
            return 6
        elif 0.45<value<=1.15:
            return 5
        elif -0.45<=value<=0.45:
            return 4
        elif -1.15<=value<-0.45:
            return 3
        elif -1.96<value<1.15:
            return 2
        elif value<=-1.96:
            return 1

def reduced_grade_index(dataframe_DW,DW_column_name,DW_column_cat):
    """根据县次划分旱涝等级 (5阶版本)

    Dryness/Wetness grade 根据所输入的旱涝距平百分比之差的分布，设定旱涝等级

    Args:
        dataframe_DW:包含旱涝距平百分比之差的时间序列表格
        DW_column_name:包含旱涝距平百分比之差的时间序列地点或编号的列名称
        DW_column_cat:包含旱涝距平百分比之差的时间序的列名称
    
    Returns:
        返回包含地点名称，年份，旱涝等级的表格
    """

    x=dataframe_DW[DW_column_cat] # 导入旱涝距平百分比之差的时间序列
    maxv=x.max() 
    minv=x.min() 
    mn=[abs(maxv),abs(minv)] #找到时间序列中的最小值和最大值的绝对值
    bound=np.float(max(mn))+1 #在所有绝对之中，找到最大的那个，+1后作为最大边界
    l=np.float(len(x)) #时间序列的总长度
 

    #print(bound,l)
    bins=[]
    a=iter(Intercut(0,bound)) #建立一个迭代器开始从0迭代

    while True:
        t0=next(a) # 随着循环每次迭代0.01的值
        if pd.value_counts(pd.cut(x,[0-t0,t0]))[0]/l>=0.4: #pd.cut将x按照[]中的大小顺序划分点分割，pd.value_counts对每段分割进行计数，如果中间段的数量占比在全部分布中不超过35%，则重复迭代
            bins.append(t0) 
            break #如果超过了35%，则记录迭代的终值并跳出循环
    #print(bins[0]) #上个迭代的终值
    l_left=pd.value_counts(pd.cut(x,[-bound,-bins[0]]))[0]
    l_right=pd.value_counts(pd.cut(x,[bins[0],bound]))[0]
    l_all=l_left+l_right
    b=iter(Intercut(bins[0],bound)) #建立新的迭代器，初值从上个迭代的终值开始
    #print(next(b))
    while True:
        try:
            t0=next(b)
        except StopIteration: #如果出现迭代中断，说明迭代到了最大端点，如果没到最大端点，则继续if语句
            df=DataFrame()
            Tbins=[0-bound,0-bins[0],bins[0],bound]
            columnname="I_"+DW_column_cat
            df[columnname]=pd.cut(x,Tbins,labels=["1","3","5"])
            df[DW_column_name]=dataframe_DW[DW_column_name] 
            df["Year"]=dataframe_DW["Year"]   
            print(pd.value_counts(pd.cut(x,Tbins)))
            return df
        if pd.value_counts(pd.cut(x,[0-t0,-bins[0]]))[0]/l_all+pd.value_counts(pd.cut(x,[bins[0],t0]))[0]/l_all>0.75: #剩余阶段1：3分
            t1=t0
            bins.append(t1)
            break
    df=DataFrame()
    Tbins=[0-bound,0-bins[1],0-bins[0],bins[0],bins[1],bound]
    columnname="I_"+DW_column_cat

    df[columnname]=pd.cut(x,Tbins,labels=["1","2","3","4","5"],duplicates="drop")
    df[DW_column_name]=dataframe_DW[DW_column_name] 
    df["Year"]=dataframe_DW["Year"]   
    print(pd.value_counts(pd.cut(x,Tbins)))
    return df

class upper_level_aggregation:
    """将县级或更低级别，整合为更高级别数据

    说明

    Attributes:
        self.df: 用于整合的dataframe
        self.list: 分组的依据list 例如 [df["Preid"],df["Year"]]
    """
    def __init__(self,lowerdf,bylist):
        self.df=lowerdf
        self.list=bylist
    def mean(self):
        self.uppergroup=self.df.groupby(self.list).mean()
        return self.uppergroup.reset_index()
    def sum(self):
        self.uppergroup=self.df.groupby(self.list).sum()
        return self.uppergroup.reset_index()


def isFloat(x):
    try:
        float(x)
        return True
    except:
        return False

    
