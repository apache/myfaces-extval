package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ViolationSeverity;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

public class ExtValCoreConfigurationViolationSeverityTestCase extends
        ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationViolationSeverityTestCase(String name)
    {
        super(name);
    }

    @ToDo(value = Priority.MEDIUM, description = "see if it is possible tpo have a web.xml parameter for this.  Otherwise remove from config.")
    public void testViolationSeverityDefault()
    {
        assertEquals(ViolationSeverity.class.getName(), ((Class) ExtValContext
                .getContext().getGlobalProperty(
                        ViolationSeverity.class.getName())).getName());

    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationViolationSeverityTestCase.class);
    }

}
