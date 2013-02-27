package org.apache.myfaces.extensions.validator.test.initializer;

import org.apache.myfaces.extensions.validator.baseval.annotation.Length;
import org.apache.myfaces.extensions.validator.baseval.annotation.Required;
import org.apache.myfaces.extensions.validator.crossval.annotation.RequiredIf;

import javax.persistence.Id;

public class DataBean
{
    @Id
    @Length(maximum = 20)
    private String property1;

    @RequiredIf(valueOf = "property1")
    private String property2;

    @Required
    private String property3;

    private String property4;

    public String getProperty1()
    {
        return property1;
    }

    public void setProperty1(String property1)
    {
        this.property1 = property1;
    }

    public String getProperty2()
    {
        return property2;
    }

    public void setProperty2(String property2)
    {
        this.property2 = property2;
    }

    public String getProperty3()
    {
        return property3;
    }

    public void setProperty3(String property3)
    {
        this.property3 = property3;
    }

    public String getProperty4()
    {
        return property4;
    }

    public void setProperty4(String property4)
    {
        this.property4 = property4;
    }
}
