package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.core.*;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

public class ExtValCoreConfigurationProjectStageResolverTestCase extends
        ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationProjectStageResolverTestCase(String name)
    {
        super(name);
    }

    public static class CustomProjectStageResolver implements
            ProjectStageResolver
    {

        public ProjectStage getCurrentProjectStage()
        {

            return ProjectStage
                    .createStage(JsfProjectStage.UnitTest.getValue());
        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter("javax.faces.PROJECT_STAGE", "SystemTest");
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
                public ProjectStageResolver projectStageResolver()
                {
                    return new CustomProjectStageResolver();
                }

            }, true);
        }
    }

    public void testProjectStageResolverDefault()
    {
        assertTrue(JsfProjectStage.is(JsfProjectStage.Production));
    }

    public void testProjectStageResolverWebXml()
    {
        assertTrue(JsfProjectStage.is(JsfProjectStage.SystemTest));
    }

    public void testProjectStageResolverCustomConfig()
    {
        assertTrue(JsfProjectStage.is(JsfProjectStage.UnitTest));
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationProjectStageResolverTestCase.class);
    }

}
