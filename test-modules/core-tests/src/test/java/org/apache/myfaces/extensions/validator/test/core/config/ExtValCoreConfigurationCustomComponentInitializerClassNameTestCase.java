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
import java.util.List;
import java.util.Map;

public class ExtValCoreConfigurationCustomComponentInitializerClassNameTestCase
        extends ExtValCoreConfigurationTestCase
{

    public ExtValCoreConfigurationCustomComponentInitializerClassNameTestCase(
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

    public static class Custom2ComponentInitializer implements
            ComponentInitializer
    {

        public void configureComponent(FacesContext facesContext,
                                       UIComponent uiComponent, Map<String, Object> metaData)
        {

        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".CUSTOM_COMPONENT_INITIALIZER",
                    CustomComponentInitializer.class.getName());
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
                public String customComponentInitializerClassName()
                {
                    return Custom2ComponentInitializer.class.getName();
                }

            }, true);
        }
    }

    public void testCustomComponentInitializerClassNameDefault()
    {
        assertEquals(0, ExtValContext.getContext().getComponentInitializers()
                .size());
    }

    public void testCustomComponentInitializerClassNameWebXml()
    {
        List<ComponentInitializer> initializers = ExtValContext.getContext()
                .getComponentInitializers();
        assertEquals(1, initializers.size());
        assertEquals(CustomComponentInitializer.class.getName(), initializers
                .get(0).getClass().getName());
    }

    public void testCustomComponentInitializerClassNameCustomConfig()
    {
        List<ComponentInitializer> initializers = ExtValContext.getContext()
                .getComponentInitializers();
        assertEquals(1, initializers.size());
        assertEquals(Custom2ComponentInitializer.class.getName(), initializers
                .get(0).getClass().getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomComponentInitializerClassNameTestCase.class);
    }

}
