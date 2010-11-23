package org.apache.myfaces.extensions.validator.test.propval.constraintsource.model;

import org.apache.myfaces.extensions.validator.core.validation.ConstraintSource;

public class ConstraintSourceAware3Bean
{
    private String property1;

    @ConstraintSource(ConstraintSourceAware3MetaDataBean.class)
    private String property2;

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
}
