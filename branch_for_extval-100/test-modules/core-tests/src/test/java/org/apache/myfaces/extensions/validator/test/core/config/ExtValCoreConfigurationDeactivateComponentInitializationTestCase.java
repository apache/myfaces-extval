package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.initializer.component.ComponentInitializer;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.util.Map;

public class ExtValCoreConfigurationDeactivateComponentInitializationTestCase
        extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationDeactivateComponentInitializationTestCase(
            String name)
    {
        super(name);
    }

    public static class CustomComponentInitializer implements
            ComponentInitializer
    {

        public void configureComponent(FacesContext facesContext,
                                       UIComponent uiComponent, Map<String, Object> metaData)
        {

        }

    }

    @Override
    protected void setUp() throws Exception
    {

        super.setUp();
        // Make sure we have one ComponentInitializer. When deactivated the
        // getComponentInitializers returns an empty list whatever registered.
        ExtValContext.getContext().addComponentInitializer(
                new CustomComponentInitializer());

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".DEACTIVATE_COMPONENT_INITIALIZATION", "true");

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
                public boolean deactivateComponentInitialization()
                {
                    return true;
                }

            }, true);
        }
    }

    public void testDeactivateComponentInitializationDefault()
    {

        assertFalse(ExtValContext.getContext().getComponentInitializers()
                .isEmpty());
    }

    public void testDeactivateComponentInitializationWebXml()
    {
        assertTrue(ExtValContext.getContext().getComponentInitializers()
                .isEmpty());
    }

    public void testDeactivateComponentInitializationCustomConfig()
    {
        assertTrue(ExtValContext.getContext().getComponentInitializers()
                .isEmpty());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationDeactivateComponentInitializationTestCase.class);
    }

}
