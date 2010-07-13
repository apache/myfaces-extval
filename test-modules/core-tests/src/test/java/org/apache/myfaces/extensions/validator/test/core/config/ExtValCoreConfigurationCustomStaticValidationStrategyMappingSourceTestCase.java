package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.validation.strategy.DefaultValidationStrategyFactory;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

public class ExtValCoreConfigurationCustomStaticValidationStrategyMappingSourceTestCase
        extends ExtValCoreConfigurationTestCase
{

    public ExtValCoreConfigurationCustomStaticValidationStrategyMappingSourceTestCase(
            String name)
    {
        super(name);
    }

    public static class CustomValidationStrategy implements ValidationStrategy
    {

        public void validate(FacesContext facesContext,
                             UIComponent uiComponent, MetaDataEntry metaDataEntry,
                             Object convertedObject)
        {

        }

    }

    public static class Custom2ValidationStrategy implements ValidationStrategy
    {

        public void validate(FacesContext facesContext,
                             UIComponent uiComponent, MetaDataEntry metaDataEntry,
                             Object convertedObject)
        {

        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(
                    ExtValInformation.WEBXML_PARAM_PREFIX
                            + ".CUSTOM_STATIC_VALIDATION_STRATEGY_MAPPING",
                    "org.apache.myfaces.extensions.validator.core.config.ExtValCoreConfigurationCustomStaticValidationStrategyMappingSourceTestCaseWebXml");

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
                public String customStaticValidationStrategyMappingSource()
                {
                    return "org.apache.myfaces.extensions.validator.core.config.ExtValCoreConfigurationCustomStaticValidationStrategyMappingSourceTestCaseCustomConfig";
                }

            }, true);
        }
    }

    public void testCustomStaticValidationStrategyMappingSourceDefault()
    {
        DefaultValidationStrategyFactory validationStrategyFactory = new DefaultValidationStrategyFactory();
        // Something that isn't available, so should return null.
        assertNull(validationStrategyFactory.create("UnitTest"));
    }

    public void testCustomStaticValidationStrategyMappingSourceWebXml()
    {
        DefaultValidationStrategyFactory validationStrategyFactory = new DefaultValidationStrategyFactory();
        assertEquals(CustomValidationStrategy.class.getName(),
                validationStrategyFactory.create("UnitTest").getClass()
                        .getName());
    }

    public void testCustomStaticValidationStrategyMappingSourceCustomConfig()
    {
        DefaultValidationStrategyFactory validationStrategyFactory = new DefaultValidationStrategyFactory();
        assertEquals(Custom2ValidationStrategy.class.getName(),
                validationStrategyFactory.create("UnitTest").getClass()
                        .getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomStaticValidationStrategyMappingSourceTestCase.class);
    }

}
