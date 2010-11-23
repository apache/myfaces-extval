package org.apache.myfaces.extensions.validator.test.propval.constraintsource.model;

import org.apache.myfaces.extensions.validator.test.propval.constraintsource.custom.CustomConstraintSource;
import org.apache.myfaces.extensions.validator.test.propval.constraintsource.custom.CustomIgnoreConstraintSource;
import org.apache.myfaces.extensions.validator.test.propval.constraintsource.custom.CustomTargetProperty;
import org.apache.myfaces.extensions.validator.test.propval.constraintsource.custom.CustomTargetPropertyId;

@CustomConstraintSource(ConstraintSourceAware6MetaDataBean.class)
public class ConstraintSourceAware6Bean
{
    private String property1;

    @CustomTargetPropertyId(Target4.class)
    private String property2;

    @CustomIgnoreConstraintSource
    private String property3;

    @CustomTargetProperty("property3")
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
}
