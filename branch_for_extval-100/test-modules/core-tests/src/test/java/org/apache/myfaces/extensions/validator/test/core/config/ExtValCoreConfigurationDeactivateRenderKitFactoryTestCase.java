package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.renderkit.ExtValRenderKit;
import org.apache.myfaces.extensions.validator.core.renderkit.ExtValRenderKitFactory;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;
import org.apache.shale.test.mock.MockRenderKit;
import org.apache.shale.test.mock.MockRenderKitFactory;

import javax.faces.render.RenderKit;
import javax.faces.render.RenderKitFactory;

public class ExtValCoreConfigurationDeactivateRenderKitFactoryTestCase extends
        ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationDeactivateRenderKitFactoryTestCase(String name)
    {
        super(name);
    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".DEACTIVATE_RENDER_KIT_FACTORY", "true");
        }
    }

    public void testDeactivateRenderKitFactoryDefault()
    {
        RenderKitFactory factory = new ExtValRenderKitFactory(
                new MockRenderKitFactory());
        RenderKit renderKit = factory.getRenderKit(facesContext,
                RenderKitFactory.HTML_BASIC_RENDER_KIT);
        assertEquals(ExtValRenderKit.class.getName(), renderKit.getClass()
                .getName());

    }

    public void testDeactivateRenderKitFactoryWebXml()
    {
        RenderKitFactory factory = new ExtValRenderKitFactory(
                new MockRenderKitFactory());
        RenderKit renderKit = factory.getRenderKit(facesContext,
                RenderKitFactory.HTML_BASIC_RENDER_KIT);
        assertEquals(MockRenderKit.class.getName(), renderKit.getClass()
                .getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationDeactivateRenderKitFactoryTestCase.class);
    }

}
