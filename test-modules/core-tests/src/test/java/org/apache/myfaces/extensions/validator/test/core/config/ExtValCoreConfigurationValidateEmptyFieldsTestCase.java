package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.initializer.component.AbstractHtmlCoreComponentsComponentInitializer;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.util.Map;

public class ExtValCoreConfigurationValidateEmptyFieldsTestCase extends
        ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationValidateEmptyFieldsTestCase(String name)
    {
        super(name);
    }

    public static class CustomComponentInitializer extends
            AbstractHtmlCoreComponentsComponentInitializer
    {

        @Override
        protected void configureRequiredAttribute(FacesContext facesContext,
                                                  UIComponent uiComponent, Map<String, Object> metaData)
        {

        }

        @Override
        public boolean validateEmptyFields()
        {
            return super.validateEmptyFields();
        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter("javax.faces.VALIDATE_EMPTY_FIELDS", "false");
        }
    }

    @Override
    protected void invokeStartupListeners()
    {
        super.invokeStartupListeners();

        // This is the normal place where the user can intervene in the startup
        // and change the config
        if (needCustomConfig())
        {

            ExtValCoreConfiguration.use(new DefaultExtValCoreConfiguration()
            {

                @Override
                public boolean validateEmptyFields()
                {
                    return false;
                }

            }, true);
        }
    }

    public void testValidateEmptyFieldsDefault()
    {
        CustomComponentInitializer initializer = new CustomComponentInitializer();
        assertTrue(initializer.validateEmptyFields());
    }

    public void testValidateEmptyFieldsWebXml()
    {
        CustomComponentInitializer initializer = new CustomComponentInitializer();
        assertFalse(initializer.validateEmptyFields());
    }

    public void testValidateEmptyFieldsCustomConfig()
    {
        CustomComponentInitializer initializer = new CustomComponentInitializer();
        assertFalse(initializer.validateEmptyFields());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationValidateEmptyFieldsTestCase.class);
    }

}
