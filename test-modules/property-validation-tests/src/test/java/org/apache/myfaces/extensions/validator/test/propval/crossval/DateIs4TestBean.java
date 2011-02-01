package org.apache.myfaces.extensions.validator.test.propval.crossval;

import org.apache.myfaces.extensions.validator.crossval.annotation.DateIs;
import org.apache.myfaces.extensions.validator.crossval.annotation.DateIsType;

import java.util.Date;

/**
 * @author Rudy De Busscher
 */
public class DateIs4TestBean
{
    private Date property1;

    @DateIs(valueOf = "property1", type = DateIsType.beforeOrSame)
    private Date property2;

    public Date getProperty1()
    {
        return property1;
    }

    public void setProperty1(Date property1)
    {
        this.property1 = property1;
    }

    public Date getProperty2()
    {
        return property2;
    }

    public void setProperty2(Date property2)
    {
        this.property2 = property2;
    }

}
