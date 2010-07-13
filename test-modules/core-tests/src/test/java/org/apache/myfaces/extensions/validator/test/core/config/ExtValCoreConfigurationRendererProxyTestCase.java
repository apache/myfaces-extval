package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.renderkit.ExtValRendererWrapper;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

import javax.faces.render.RenderKit;
import javax.faces.render.Renderer;

public class ExtValCoreConfigurationRendererProxyTestCase extends
        ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationRendererProxyTestCase(String name)
    {
        super(name);
    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
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

            }, true);
        }
    }

    public void testRendererProxyDefault()
    {
        RenderKit kit = facesContext.getRenderKit();
        Renderer renderer = kit.getRenderer("javax.faces.Input", "javax.faces.Text");
        assertEquals(ExtValRendererWrapper.class.getName(), renderer.getClass().getName());
    }

    @ToDo(Priority.HIGH)
    public void testRendererProxyWebXml()
    {
    }

    @ToDo(Priority.HIGH)
    public void testRendererProxyCustomConfig()
    {
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(ExtValCoreConfigurationRendererProxyTestCase.class);
    }

}
