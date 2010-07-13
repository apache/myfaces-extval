package org.apache.myfaces.extensions.validator.test.core.config.support;

import org.apache.myfaces.extensions.validator.core.validation.ConstraintSource;
import org.apache.myfaces.extensions.validator.core.validation.IgnoreConstraintSource;
import org.apache.myfaces.extensions.validator.core.validation.TargetProperty;
import org.apache.myfaces.extensions.validator.core.validation.TargetPropertyId;

@ConstraintSource(ConstraintSourceAwareMetaDataBean.class)
public class ConstraintSourceAwareBean
{
    @IgnoreConstraintSource
    private String property1;

    @CustomIgnoreConstraintSource
    private String property2;

    @TargetProperty(value = "test1")
    private String property3;

    @CustomTargetProperty(value = "test2")
    private String property4;

    @TargetPropertyId(value = ConstraintSource.class)
    private String property5;

    @CustomTargetPropertyId(value = CustomConstraintSource.class)
    private String property6;

    public String getProperty1()
    {
        return property1;
    }

    public void setProperty1(String property1)
    {
        this.property1 = property1;
    }

    @CustomConstraintSource(ConstraintSourceAware2MetaDataBean.class)
    public String getProperty2()
    {
        return property2;
    }

    public void setProperty2(String property2)
    {
        this.property2 = property2;
    }
}


