package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.validation.parameter.DisableClientSideValidation;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

public class ExtValCoreConfigurationDisableClientSideValidationValidationParameterTestCase
        extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationDisableClientSideValidationValidationParameterTestCase(
            String name)
    {
        super(name);
    }

    public static interface CustomDisableClientSideValidation extends
            DisableClientSideValidation
    {

    }

    @ToDo(value = Priority.MEDIUM, description = "Allow for web xml configuration")
    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
        }
    }

    public void testDisableClientSideValidationValidationParameterDefault()
    {

        assertEquals(DisableClientSideValidation.class.getName(),
                ((Class) ExtValContext.getContext().getGlobalProperty(
                        DisableClientSideValidation.class.getName())).getName());
    }

    @ToDo(Priority.HIGH)
    public void testDisableClientSideValidationValidationParameterWebXml()
    {
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationDisableClientSideValidationValidationParameterTestCase.class);
    }

}
